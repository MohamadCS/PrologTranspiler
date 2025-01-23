#include "CodeGen.hpp"
#include "Utils.hpp"
#include <any>
#include <cctype>
#include <format>
#include <map>
#include <optional>
#include <sstream>
#include <string>
#include <vector>

namespace Prolog::CodeGen {

static std::string addTabs(std::string str, std::size_t num) {
    std::stringstream result;
    for (int i = 0; i < num; ++i) {
        result << '\t';
    }
    result << str;
    return result.str();
}

std::vector<std::string> CodeGenVisitor::getCodeBuffer() const {
    std::vector<std::string> result;
    result.push_back(getModulesCode());
    result.insert(result.end(), m_importedModules.begin(), m_importedModules.end());
    result.insert(result.end(), m_lambdasBuffer.begin(), m_lambdasBuffer.end());
    result.insert(result.end(), m_codeBuffer.begin(), m_codeBuffer.end());
    return result;
}

void CodeGenVisitor::emit(std::string&& line) {
    if (m_formatOutput) {
        line = addTabs(line, m_currentTabs);
    }
    if (m_insideLambda) {
        m_lambdasBuffer.push_back(line);
    } else {
        m_codeBuffer.push_back(line);
    }
}

std::string CodeGenVisitor::genPredName(std::string funcName) {
    if (funcName.empty()) {
        // ERROR : EMPTY string;
        LOG("Empty func name");
    }
    funcName[0] = std::tolower(funcName[0]);
    return funcName;
}

std::string CodeGenVisitor::genVar() {
    return std::format("_var{}", m_varCtr++);
};

static std::string funcToPredCode(const std::string& predName, const std::vector<std::string>& argsNames,
                                  const std::string& returnVar, std::optional<std::string> ns) {
    std::stringstream argsTuple;
    for (auto& argStr : argsNames) {
        argsTuple << argStr << ",";
    }

    argsTuple << returnVar;

    std::string nsStr = ns.has_value() ? std::format("{}:", ns.value()) : "";

    std::string result = std::format("{}{}({}) :- ", std::move(nsStr), predName, argsTuple.str());
    return result;
}

std::any CodeGenVisitor::visitIf(prologParser::IfContext* ctx) {
    LOG("If");
    CHECK_NULL(ctx);

    auto* conditionTerm = ctx->term();

    if (ctx->if_head()) {
        const auto& bindingVec = ctx->if_head()->binding();

        for (auto* pBindingCtx : bindingVec) {
            visit(pBindingCtx);
        }
    }

    emit(std::format("( ({} -> (", conditionTerm->getText()));
    m_currentTabs++;

    std::any result = visit(ctx->cond_tuple());
    emit("true);true)");
    m_currentTabs--;
    emit("),");

    return result;
}

std::any CodeGenVisitor::visitIf_else(prologParser::If_elseContext* ctx) {
    LOG("If else");
    CHECK_NULL(ctx);

    Node node;
    node.var = genVar();

    if (ctx->if_head()) {
        const auto& bindingVec = ctx->if_head()->binding();

        for (auto* pBindingCtx : bindingVec) {
            visit(pBindingCtx);
        }
    }

    auto* conditionTerm = ctx->term();
    emit(std::format("( {} -> (", conditionTerm->getText()));
    m_currentTabs++;

    auto* ifBodyTuple = ctx->cond_tuple()[0];
    auto* elseBodyTuple = ctx->cond_tuple()[1];

    std::any returnValue;

    Node ifRetValue = std::any_cast<Node>(visit(ifBodyTuple));
    emit(std::format("{} = {}", node.var, ifRetValue.var));
    m_currentTabs--;
    emit(")");
    emit(";(");
    m_currentTabs++;

    Node elseRetValue = std::any_cast<Node>(visit(elseBodyTuple));
    emit(std::format("{} = {}", node.var, elseRetValue.var));
    m_currentTabs--;
    emit(")");

    emit("),");

    return node;
}

std::any CodeGenVisitor::visitBinding(prologParser::BindingContext* ctx) {
    LOG("Binding");
    CHECK_NULL(ctx);

    Node node = std::any_cast<Node>(visit(ctx->expr()));
    std::string bindedVarName = ctx->children[0]->getText();

    std::string result = std::format("{} = {},", bindedVarName, node.var);

    emit(std::move(result));

    Node bindingNode;
    bindingNode.var = bindedVarName;

    return bindingNode;
}

/*
 * If the function gets F(args ...) then it generates
 * f(Vars(args ...), Result)
 * returns Node(Result)
 *
 */
std::any CodeGenVisitor::visitInvoc(prologParser::InvocContext* ctx) {
    LOG("Invocation");
    CHECK_NULL(ctx);

    auto* tuple = ctx->tuple();

    std::string funcName = ctx->VARIABLE()->getText();

    bool isLambdaInvoc = m_funcNames.find(funcName) == m_funcNames.end() && !ctx->namespace_();

    // Evaluate arguments, and add them to the vector.
    std::vector<std::string> predicateArgs;
    for (auto* child : tuple->tuple_entry()) {
        Node node = std::any_cast<Node>(visit(child));
        predicateArgs.push_back(node.var);
    }

    Node invocNode;

    invocNode.var = genVar();
    predicateArgs.push_back(invocNode.var); // Result var

    std::string namespaceStr= getNameSpace(ctx);

    std::string predicateName;
    if (m_funcNames.find(funcName) != m_funcNames.end()) {
        predicateName = genPredName(funcName);
    } else if (ctx->namespace_()) {
        predicateName = genPredName(ctx->VARIABLE()->getText());
    } else {
        predicateName = funcName;
    }

    auto itemFormatFunc = [](decltype(predicateArgs.begin()) it) { return *it; };
    std::string args = Prolog::Utility::convertContainerToListStr<decltype(predicateArgs.begin())>(
        predicateArgs.begin(), predicateArgs.end(), itemFormatFunc);
    std::string predicateInvoc;

    if (isLambdaInvoc) {
        predicateInvoc = std::format("call({},{}),", predicateName, args);
    } else {
        predicateInvoc = std::format("{}{}({}),", namespaceStr, predicateName, args);
    }

    emit(std::move(predicateInvoc));

    return invocNode;
}

std::any CodeGenVisitor::visitTuple(prologParser::TupleContext* ctx) {
    CHECK_NULL(ctx);
    LOG("START TUPLE");

    const auto isVanishingList = Utility::isVanishingEntryList(ctx);

    // Add non-vanishing results to the tuple evaluation

    int i = 0;
    std::vector<std::string> varValuesVec;

    for (auto* child : ctx->tuple_entry()) {
        std::any returnValue = visit(child);

        if (returnValue.has_value()) {
            Node value = std::any_cast<Node>(returnValue);

            if (value.isPredicate) {
                emit(std::format("{},", child->getText()));
                continue;
            }
        }

        if (!isVanishingList[i++]) {
            if (returnValue.has_value()) {
                Node value = std::any_cast<Node>(returnValue);

                if (value.isEmptyTuple) {
                    continue;
                }

                varValuesVec.push_back(value.var);
            }
        }
    }

    Node tupleNode;

    tupleNode.var = genVar();

    auto itemFormatFunc = [](decltype(varValuesVec.begin()) it) { return *it; };

    std::string resultStr;

    // If the tuple is empty then dont store it in a variable, and return an empty node.
    //

    if (varValuesVec.empty()) {
        tupleNode.isEmptyTuple = true;
    }

    if (varValuesVec.size() == 1) {
        resultStr = std::format("{} = {},", tupleNode.var, varValuesVec[0]);
    } else {
        resultStr = std::format("{} = tuple( {} ),", tupleNode.var,
                                Prolog::Utility::convertContainerToListStr<decltype(varValuesVec.begin())>(
                                    varValuesVec.begin(), varValuesVec.end(), itemFormatFunc));
    }

    emit(std::move(resultStr));

    LOG("END TUPLE");
    return tupleNode;
}

void CodeGenVisitor::setFuncNames(const std::vector<std::string>& vec) {
    m_funcNames = std::set<std::string>(vec.begin(), vec.end());
}

std::any CodeGenVisitor::visitModule(prologParser::ModuleContext* ctx) {
    CHECK_NULL(ctx);
    std::string moduleName = ctx->namespace_()->getText();
    if (m_modules.find(moduleName) == m_modules.end()) {
        m_modules.insert({moduleName, {}});
    }
    m_currentModule = moduleName;
    auto result = visitChildren(ctx);
    m_currentModule = std::nullopt;
    return result;
}

std::any CodeGenVisitor::visitFunc_def(prologParser::Func_defContext* ctx) {
    LOG("Func def");
    CHECK_NULL(ctx);

    const std::string funcName = ctx->VARIABLE()->getText();

    /*Preparing the predicate*/
    Predicate currentPredicate;

    currentPredicate.name = genPredName(funcName);

    auto argsVec = ctx->func_args()->VARIABLE();
    for (auto* pVarCtx : argsVec) {
        currentPredicate.args.push_back(pVarCtx->getText());
    }
    currentPredicate.returnVar = genVar();

    m_currentPredicate = std::move(currentPredicate);

    if (m_currentModule.has_value() && ctx->public_()) {
        m_modules.at(m_currentModule.value()).push_back(m_currentPredicate.value());
    }

    /****/

    // Emit func(args) :-
    emit(funcToPredCode(m_currentPredicate->name, m_currentPredicate->args, m_currentPredicate->returnVar,
                        m_currentModule));
    m_currentTabs++;

    // Fill the predicate body.
    Node result = std::any_cast<Node>(visit(ctx->tuple()));

    std::string resultStmt = std::format("{} = {}", m_currentPredicate->returnVar, result.var);

    emit(std::move(resultStmt));

    // Finish the predicate.
    emit(".");

    m_currentTabs--;

    emit("\n");
    m_varCtr = 0;
    m_lambdaCtr = 0;
    return {};
}

std::any CodeGenVisitor::visitVariable(prologParser::VariableContext* ctx) {
    CHECK_NULL(ctx);
    LOG("Var");

    Node node;

    node.var = ctx->getText();

    return node;
}

// TODO: Add the rest of the aithmetic operators.
static inline bool isArith(const std::string& op) {
    const std::set<std::string> arithOps{
        "+",
        "-",
        "*",
        "/",
    };

    return arithOps.find(op) != arithOps.end();
}
std::any CodeGenVisitor::visitBinary_operator(prologParser::Binary_operatorContext* ctx) {
    CHECK_NULL(ctx);

    LOG("Bin op");
    std::string op = ctx->operator_()->getText();

    if (!isArith(op)) {
        return {};
    }

    Node node;

    node.var = genVar();

    std::string arithExp = ctx->getText();

    emit(std::format("{} is {},", node.var, arithExp));

    return node;
}

Node CodeGenVisitor::generateArithCode(antlr4::RuleContext* ctx) {
    Node node;

    node.var = genVar();

    std::string arithExp = ctx->getText();

    emit(std::format("{} is {},", node.var, arithExp));

    return node;
}

std::any CodeGenVisitor::visitUnary_operator(prologParser::Unary_operatorContext* ctx) {
    CHECK_NULL(ctx);
    LOG("Un op");

    std::string op = ctx->operator_()->getText();

    if (!isArith(op)) {
        return {};
    }

    return generateArithCode(ctx);
}

std::any CodeGenVisitor::visitFloat(prologParser::FloatContext* ctx) {
    CHECK_NULL(ctx);
    LOG("Float");
    return generateArithCode(ctx);
}

std::any CodeGenVisitor::visitInteger_term(prologParser::Integer_termContext* ctx) {
    CHECK_NULL(ctx);
    LOG("Int");
    return generateArithCode(ctx);
}

std::any CodeGenVisitor::visitExpr(prologParser::ExprContext* ctx) {
    CHECK_NULL(ctx);
    LOG("Expr");

    std::any value = visit(ctx->children[0]);

    // For now, if it does not have value, then its a term but not a variable.
    if (value.has_value()) {
        LOG(ctx->getText());
        LOG("END EXP");
        return value;
    } else if (ctx->tuple()) {
        LOG(ctx->getText());
        return {};
    } else {
        emit(std::format("{},", ctx->getText()));
        return {};
    }
}

std::any CodeGenVisitor::visitList_term(prologParser::List_termContext* ctx) {
    CHECK_NULL(ctx);

    Node node;

    node.var = genVar();

    emit(std::format("{} = {},", node.var, ctx->getText()));

    return node;
}

std::any CodeGenVisitor::visitCompound_term(prologParser::Compound_termContext* ctx) {
    CHECK_NULL(ctx);

    Node node;

    node.var = genVar();

    emit(std::format("{} = {},", node.var, ctx->getText()));
    node.isPredicate = true;

    return node;
}

std::any CodeGenVisitor::visitName(prologParser::NameContext* ctx) {
    CHECK_NULL(ctx);

    Node node;

    node.var = genVar();

    node.isPredicate = true;

    return node;
}

std::any CodeGenVisitor::visitDirective(prologParser::DirectiveContext* ctx) {
    CHECK_NULL(ctx);

    emit(ctx->getText());
    emit("\n");
    return {};
}

std::any CodeGenVisitor::visitClause(prologParser::ClauseContext* ctx) {
    CHECK_NULL(ctx);
    emit(ctx->getText());
    emit("\n");
    return {};
}

std::any CodeGenVisitor::visitLambda(prologParser::LambdaContext* ctx) {
    CHECK_NULL(ctx);
    m_insideLambda = true;

    Node lambdaNode;

    lambdaNode.var = genVar();

    auto oldTabs = m_currentTabs;
    m_currentTabs = 0;

    std::string nsStr;
    if (m_currentModule.has_value()) {
        nsStr = std::format("{}:", m_currentModule.value());
    }

    std::string lambdaName = std::format("{}{}_lambda{}", nsStr, m_currentPredicate->name, m_lambdaCtr);

    std::stringstream argsStr;

    std::string resultVar = genVar();

    for (auto* pArgCtx : ctx->func_args()->VARIABLE()) {
        argsStr << pArgCtx->getText() << ",";
    }

    argsStr << resultVar;

    emit(std::format("{}({}) :- ", lambdaName, argsStr.str()));
    m_currentTabs++;

    Node resultNode = std::any_cast<Node>(visit(ctx->tuple()));

    emit(std::format("{} = {}", resultVar, resultNode.var));
    emit(".");
    emit("\n");

    m_insideLambda = false;

    m_currentTabs = oldTabs;

    emit(std::format("{} = {},", lambdaNode.var, lambdaName));

    return lambdaNode;
}

std::string CodeGenVisitor::getModulesCode() const {
    std::stringstream modulesCode;
    for (const auto& [ns, funcList] : m_modules) {
        modulesCode << std::format(":- module(\n");
        modulesCode << std::format("\t{},\n", ns);
        modulesCode << std::format("\t[\n");

        auto itemFormatFunc = [](decltype(funcList.begin()) it) {
            std::string result = std::format("\t\t{}/{}", it->name, it->args.size() + 1);
            return result;
        };

        std::string funcStr = Prolog::Utility::convertContainerToListStr<decltype(funcList.begin())>(
            funcList.begin(), funcList.end(), itemFormatFunc, ",\n");

        modulesCode << '\n';
        modulesCode << funcStr;
        modulesCode << std::format("\t]\n");
        modulesCode << std::format(")\n");
        modulesCode << std::format(".\n");
    }

    return modulesCode.str();
}

std::any CodeGenVisitor::visitImport_modules(prologParser::Import_modulesContext* ctx) {
    CHECK_NULL(ctx);
    const auto& fileNameCtxVec = ctx->QUOTED();

    m_importedModules.reserve(fileNameCtxVec.size());

    auto generatePlFileName  = [](const std::string& name){
        std::string result = "'";
        result += name.substr(1,name.size() - 2); 
        result +=  ".pl\'";
        return result;
    };

    for (auto& pStringCtx : fileNameCtxVec) {
        m_importedModules.push_back(std::format(":- use_module({}).", generatePlFileName(pStringCtx->getText())));
    }

    return {};
}

std::string CodeGenVisitor::getNameSpace(prologParser::InvocContext* ctx) const {
    CHECK_NULL(ctx);

    if (ctx->namespace_()) {// external / internal
        return std::format("{}:", ctx->namespace_()->getText());
    } else {
        return m_currentModule.has_value() ? std::format("{}:",m_currentModule.value()) : "";
    }
}

} // namespace Prolog::CodeGen
