#include "CodeGen.hpp"
#include "Utils.hpp"
#include <any>
#include <cctype>
#include <format>
#include <map>
#include <optional>
#include <ranges>
#include <sstream>
#include <string>
#include <vector>

namespace Prolog::CodeGen {

std::string CodeGenVisitor::genTypeCode(std::string varName, prologParser::TypeContext* pTypeCtx) {
    std::string nsStr;

    if (pTypeCtx->namespace_()) {
        nsStr = pTypeCtx->namespace_()->getText() + ":";
    }
    std::string typeStr = pTypeCtx->atomic_name()->getText();

    if (pTypeCtx->getText()[pTypeCtx->getText().size() - 1] == '?') {
        return std::format("({} = nil ; {}{}({})),", varName, nsStr, typeStr, varName);
    } else {
        return std::format("{}{}({}),", nsStr, typeStr, varName);
    }
}

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

    std::stringstream typeDecl;

    argsTuple << returnVar;

    std::string nsStr = ns.has_value() ? std::format("{}:", ns.value()) : "";

    std::string result;
    if (predName != "main") {
        result = std::format("{}({}) :- ", predName, argsTuple.str());
    } else {
        result = "main :- ";
    }
    return result;
}

std::any CodeGenVisitor::visitType_def(prologParser::Type_defContext* ctx) {
    CHECK_NULL(ctx);
    auto targetVar = genVar();

    const auto& typeName = ctx->atomic_name()->getText();
    const auto& typeCtxVec = ctx->type();
    emit(std::format("{}({}) :- ", typeName, targetVar));
    m_currentTabs++;

    std::vector<std::string> membersVec;
    membersVec.reserve(typeCtxVec.size());

    for (int i = 0; i < typeCtxVec.size(); ++i) {
        membersVec.push_back(genVar());
    }

    std::string membersStr = Utility::convertContainerToListStr<decltype(membersVec.begin())>(
        membersVec.begin(), membersVec.end(), [](decltype(membersVec.begin()) it) { return *it; });

    std::string nsStr;

    if (m_currentModule.has_value()) {
        nsStr = m_currentModule.value() + ":";
    }

    emit(std::format("{} = {}{}({}),", targetVar, nsStr, typeName, membersStr));

    for (const auto& [varName, pTypeCtx] : std::ranges::views::zip(membersVec, typeCtxVec)) {
        emit(genTypeCode(varName, pTypeCtx));
    }

    emit("!");
    emit(".");

    if (m_currentModule.has_value() && ctx->public_()) {
        Predicate predicate;
        predicate.name = typeName;
        m_modules.at(m_currentModule.value()).push_back(std::move(predicate));
    }

    m_currentTabs--;

    emit(std::format("{}(_) :- write(\"Type mismatch\"),fail.", typeName));

    m_varCtr = 0;
    return {};
}

std::any CodeGenVisitor::visitIf(prologParser::IfContext* ctx) {
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
    CHECK_NULL(ctx);

    Node node = std::any_cast<Node>(visit(ctx->expr()));
    std::string bindedVarName;

    std::vector<std::string> varNamesVec;

    varNamesVec.reserve(ctx->binding_var().size());

    // note that alias arg is not an option here, we suppose that we've processed it before.
    for (auto* pBindingVarCtx : ctx->binding_var()) {
        varNamesVec.push_back(pBindingVarCtx->var_decl()->VARIABLE()->getText());
    }

    if (varNamesVec.size() == 1) {
        emit(std::format("{} = {},", varNamesVec[0], node.var));
    } else {
        std::string vecListStr = Prolog::Utility::convertContainerToListStr<decltype(varNamesVec.begin())>(
            varNamesVec.begin(), varNamesVec.end(), [](decltype(varNamesVec.begin()) it) { return *it; });

        emit(std::format("tuple({}) = {},", std::move(vecListStr), node.var));
    }

    for (auto* pBindingVarCtx : ctx->binding_var()) {
        if (pBindingVarCtx->var_decl()->type()) {
            emit(genTypeCode(pBindingVarCtx->var_decl()->VARIABLE()->getText(), pBindingVarCtx->var_decl()->type()));
        }
    }

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
    CHECK_NULL(ctx);

    auto* tuple = ctx->tuple();

    std::string funcName = ctx->VARIABLE()->getText();

    bool isLambdaInvoc =
        m_funcNames.find(funcName) == m_funcNames.end() && !ctx->namespace_() && (funcName != "RunTests");

    // Evaluate arguments, and add them to the vector.
    std::vector<std::string> predicateArgs;
    for (auto* child : tuple->tuple_entry()) {
        Node node = std::any_cast<Node>(visit(child));
        predicateArgs.push_back(node.var);
    }

    Node invocNode;

    invocNode.var = genVar();
    predicateArgs.push_back(invocNode.var); // Result var

    std::string namespaceStr = getNameSpace(ctx);

    std::string predicateName;
    if (m_funcNames.find(funcName) != m_funcNames.end() || funcName == "RunTests") {
        predicateName = genPredName(funcName);
    } else if (ctx->namespace_()) {
        predicateName = genPredName(ctx->VARIABLE()->getText());
    } else {
        predicateName = funcName;
    }

    std::string args = Prolog::Utility::convertContainerToListStr<decltype(predicateArgs.begin())>(
        predicateArgs.begin(), predicateArgs.end(), [](decltype(predicateArgs.begin()) it) { return *it; });
    std::string predicateInvoc;

    if (isLambdaInvoc) {
        predicateInvoc = std::format("call({},{}),", predicateName, std::move(args));
    } else {
        predicateInvoc = std::format("{}{}({}),", namespaceStr, predicateName, std::move(args));
    }

    emit(std::move(predicateInvoc));

    return invocNode;
}

std::any CodeGenVisitor::visitTuple(prologParser::TupleContext* ctx) {
    CHECK_NULL(ctx);

    const auto isVanishingList = Utility::isVanishingEntryList(ctx);

    // Add non-vanishing results to the tuple evaluation

    int i = 0;
    std::vector<std::string> varValuesVec;

    for (auto* child : ctx->tuple_entry()) {
        std::any returnValue = visit(child);

        if (returnValue.has_value()) {
            Node value = std::any_cast<Node>(returnValue);

            if (value.isPredicate) {
                emit(std::format("{},", value.predicateText));
                ++i;
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

    std::string resultStr;

    // If the tuple is empty then dont store it in a variable, and return an empty node.
    if (varValuesVec.empty()) {
        tupleNode.isEmptyTuple = true;
    }

    if (varValuesVec.size() == 1) {
        resultStr = std::format("{} = {},", tupleNode.var, varValuesVec[0]);
    } else {
        resultStr = std::format(
            "{} = tuple( {} ),", tupleNode.var,
            Prolog::Utility::convertContainerToListStr<decltype(varValuesVec.begin())>(
                varValuesVec.begin(), varValuesVec.end(), [](decltype(varValuesVec.begin()) it) { return *it; }));
    }

    emit(std::move(resultStr));

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
    CHECK_NULL(ctx);

    const std::string funcName = ctx->VARIABLE()->getText();

    /*Preparing the predicate*/
    Predicate currentPredicate;

    currentPredicate.name = genPredName(funcName);

    auto argsVec = ctx->func_args()->var_decl();
    for (auto* pVarCtx : argsVec) {
        currentPredicate.args.push_back(pVarCtx->VARIABLE()->getText());
    }
    currentPredicate.returnVar = genVar();

    m_currentPredicate = std::move(currentPredicate);

    m_predicateStack.push(m_currentPredicate.value());

    if (m_currentModule.has_value() && ctx->public_()) {
        m_modules.at(m_currentModule.value()).push_back(m_currentPredicate.value());
    }

    /****/

    // Emit func(args) :-
    emit(funcToPredCode(m_currentPredicate->name, m_currentPredicate->args, m_currentPredicate->returnVar,
                        m_currentModule));

    m_currentTabs++;

    for (auto* pVarCtx : argsVec) {
        if (pVarCtx->type()) {
            emit(genTypeCode(pVarCtx->VARIABLE()->getText(), pVarCtx->type()));
        }
    }

    // Fill the predicate body.
    Node result = std::any_cast<Node>(visit(ctx->tuple()));

    std::string resultStmt = std::format("{} = {}", m_currentPredicate->returnVar, result.var);

    emit(std::move(resultStmt));

    // Finish the predicate.
    emit(".");

    m_currentTabs--;
    m_predicateStack.pop();

    emit("\n");
    m_varCtr = 0;
    m_lambdaCtr = 0;
    return {};
}

std::any CodeGenVisitor::visitVariable(prologParser::VariableContext* ctx) {
    CHECK_NULL(ctx);

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

Node CodeGenVisitor::genArithCode(antlr4::RuleContext* ctx) {
    Node node;

    node.var = genVar();

    std::string arithExp = ctx->getText();

    emit(std::format("{} is {},", node.var, arithExp));

    return node;
}

std::any CodeGenVisitor::visitUnary_operator(prologParser::Unary_operatorContext* ctx) {
    CHECK_NULL(ctx);

    std::string op = ctx->operator_()->getText();

    if (!isArith(op)) {
        return {};
    }

    return genArithCode(ctx);
}

std::any CodeGenVisitor::visitFloat(prologParser::FloatContext* ctx) {
    CHECK_NULL(ctx);
    return genArithCode(ctx);
}

std::any CodeGenVisitor::visitInteger_term(prologParser::Integer_termContext* ctx) {
    CHECK_NULL(ctx);
    return genArithCode(ctx);
}

std::any CodeGenVisitor::visitExpr(prologParser::ExprContext* ctx) {
    CHECK_NULL(ctx);

    std::any value = visit(ctx->children[0]);

    // For now, if it does not have value, then its a term but not a variable.
    if (value.has_value()) {
        return value;
    } else if (ctx->tuple()) {
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

    std::vector<std::string> entriesVec;

    // Go over the list expr, if they are terms then generate the same code
    // otherwise visit then add the returnes var instead of the expression.c
    if (ctx->expr_list()) {
        const auto& expCtxVec = ctx->expr_list()->expr();
        entriesVec.reserve(expCtxVec.size());

        for (auto* pExprCtx : ctx->expr_list()->expr()) {
            if (pExprCtx->term()) {
                entriesVec.push_back(pExprCtx->getText());
            } else {
                auto expResult = visit(pExprCtx);
                if (expResult.has_value()) {
                    entriesVec.push_back(std::any_cast<Node>(expResult).var);
                }
            }
        }
    }

    std::string exprListStr = Prolog::Utility::convertContainerToListStr<decltype(entriesVec.begin())>(
        entriesVec.begin(), entriesVec.end(), [](decltype(entriesVec.begin()) it) { return *it; });

    if (auto* pExprCtx = ctx->expr(); pExprCtx != nullptr) {

        std::string exprStr;

        if (pExprCtx->term()) {
            exprStr = pExprCtx->getText();
        } else {
            auto expResult = visit(pExprCtx);
            if (expResult.has_value()) {
                exprStr = std::any_cast<Node>(expResult).var;
            }
        }

        exprListStr += std::format(" | {}", exprStr);
    }

    emit(std::format("{} = [ {} ],", node.var, std::move(exprListStr)));

    return node;
}

std::any CodeGenVisitor::visitCompound_term(prologParser::Compound_termContext* ctx) {
    CHECK_NULL(ctx);

    Node node;

    node.var = genVar();

    std::vector<std::string> entriesVec;

    // Go over the list expr, if they are terms then generate the same code
    // otherwise visit then add the returnes var instead of the expression.c
    if (ctx->expr_list()) {
        const auto& expCtxVec = ctx->expr_list()->expr();
        entriesVec.reserve(expCtxVec.size());

        for (auto* pExprCtx : ctx->expr_list()->expr()) {
            if (pExprCtx->term()) {
                entriesVec.push_back(pExprCtx->getText());
            } else {
                auto expResult = visit(pExprCtx);
                if (expResult.has_value()) {
                    entriesVec.push_back(std::any_cast<Node>(expResult).var);
                }
            }
        }
    }

    std::string exprListStr = Prolog::Utility::convertContainerToListStr<decltype(entriesVec.begin())>(
        entriesVec.begin(), entriesVec.end(), [](decltype(entriesVec.begin()) it) { return *it; });

    node.predicateText = std::format("{}({})", ctx->atom()->getText(), std::move(exprListStr));

    emit(std::format("{} = {},", node.var, node.predicateText));

    node.isPredicate = true;

    return node;
}

std::any CodeGenVisitor::visitAtom_term(prologParser::Atom_termContext* ctx) {
    CHECK_NULL(ctx);

    if (auto* pAtomExprCtx = ctx->atom()->atom_expr(); pAtomExprCtx != nullptr) {

        auto returnValue = visit(ctx->atom());
        if (returnValue.has_value()) {
            return returnValue;
        } else {
            Node node;
            node.var = genVar();
            emit(std::format("{} = {},", node.var, pAtomExprCtx->getText()));
            return node;
        }
    }

    return {};
}

std::any CodeGenVisitor::visitName(prologParser::NameContext* ctx) {
    CHECK_NULL(ctx);

    Node node;

    node.var = genVar();
    node.isPredicate = true;
    node.predicateText = ctx->getText();

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

    ///// Tabs
    auto oldTabs = m_currentTabs;
    m_currentTabs = 0;
    //////

    std::string nsStr;
    if (m_currentModule.has_value()) {
        nsStr = std::format("{}:", m_currentModule.value());
    }

    Predicate lambdaPredicate;
    std::string lambdaName = std::format("{}{}_lambda{}", nsStr, m_currentPredicate->name, m_lambdaCtr);
    lambdaPredicate.name = lambdaName;

    std::stringstream argsStr;
    std::string resultVar = genVar();

    for (auto* pArgCtx : ctx->func_args()->var_decl()) {
        argsStr << pArgCtx->VARIABLE()->getText() << ",";
    }

    argsStr << resultVar;

    lambdaPredicate.returnVar = resultVar;

    m_predicateStack.push(lambdaPredicate);

    emit(std::format("{}({}) :- ", lambdaName, argsStr.str()));

    m_currentTabs++;

    std::stringstream argTypes;

    for (auto* pArgCtx : ctx->func_args()->var_decl()) {
        if (pArgCtx->type()) {
            emit(genTypeCode(pArgCtx->VARIABLE()->getText(), pArgCtx->type()));
        }
    }

    Node resultNode = std::any_cast<Node>(visit(ctx->tuple()));

    emit(std::format("{} = {}", resultVar, resultNode.var));
    emit(".");
    emit("\n");

    m_insideLambda = false;

    m_currentTabs = oldTabs;

    emit(std::format("{} = {},", lambdaNode.var, lambdaName));

    m_predicateStack.pop();

    return lambdaNode;
}

std::any CodeGenVisitor::visitMatch_stmt(prologParser::Match_stmtContext* ctx) {
    CHECK_NULL(ctx);

    Node returnNode;
    returnNode.var = genVar();

    auto matchExpr = visit(ctx->expr());

    if (!matchExpr.has_value()) {
        std::cerr << "It must have a value" << '\n';
    }

    auto matchExprNode = std::any_cast<Node>(matchExpr);

    const auto& matchEntryCtxVec = ctx->match_entry();

    std::vector<std::string> varVec;
    varVec.reserve(matchEntryCtxVec.size());

    // For each possible match, init the variable first.
    for (auto* matchEntryCtx : matchEntryCtxVec) {
        auto matchExprEntryResult = std::any_cast<Node>(visit(matchEntryCtx->expr()));
        varVec.push_back(matchExprEntryResult.var);
    }

    emit("("); // Start of match

    for (const auto& [entryVar, matchEntryCtx] : std::ranges::views::zip(varVec, matchEntryCtxVec)) {
        emit(std::format("{} = {} -> ", matchExprNode.var, entryVar));
        auto matchExprEntryResult = visit(matchEntryCtx->tuple_entry());
        if (matchExprEntryResult.has_value()) {
            auto matchExprEntryNode = std::any_cast<Node>(matchExprEntryResult);
            emit(std::format("{} = {}", returnNode.var, matchExprEntryNode.var));
        }

        emit(";");
    }


    // If else exists print its code
    if (ctx->match_else()) {
        auto otherwiseResult = visit(ctx->match_else()->tuple_entry());

        if (otherwiseResult.has_value()) {
            auto otherwiseNode = std::any_cast<Node>(otherwiseResult);
            emit(std::format("{} = {}", returnNode.var, otherwiseNode.var));
        }
    } else {
        emit(std::format("{} = nil", returnNode.var));
    }

    emit("),"); // End of match

    return returnNode;
}

std::string CodeGenVisitor::getModulesCode() const {
    std::stringstream modulesCode;
    for (const auto& [ns, funcList] : m_modules) {
        modulesCode << std::format(":- module(\n");
        modulesCode << std::format("\t{},\n", ns);
        modulesCode << std::format("\t[\n");

        std::string funcStr = Prolog::Utility::convertContainerToListStr<decltype(funcList.begin())>(
            funcList.begin(), funcList.end(),
            [](decltype(funcList.begin()) it) {
                std::string result = std::format("\t\t{}/{}", it->name, it->args.size() + 1);
                return result;
            },
            ",\n");

        modulesCode << std::move(funcStr);
        modulesCode << '\n';
        modulesCode << "\t]\n";
        modulesCode << ")\n";
        modulesCode << ".\n";
    }

    return modulesCode.str();
}

std::any CodeGenVisitor::visitImport_modules(prologParser::Import_modulesContext* ctx) {
    CHECK_NULL(ctx);

    const auto& fileNameCtxVec = ctx->QUOTED();

    m_importedModules.reserve(fileNameCtxVec.size());

    auto generatePlFileName = [](const std::string& name) {
        std::string result = "'";
        result += name.substr(1, name.size() - 2);
        result += ".pl\'";
        return result;
    };

    for (auto& pStringCtx : fileNameCtxVec) {
        m_importedModules.push_back(std::format(":- use_module({}).", generatePlFileName(pStringCtx->getText())));
    }

    return {};
}

std::string CodeGenVisitor::getNameSpace(prologParser::InvocContext* ctx) const {
    CHECK_NULL(ctx);

    if (ctx->namespace_()) { // external / internal
        return std::format("{}:", ctx->namespace_()->getText());
    } else {
        return m_currentModule.has_value() ? std::format("{}:", m_currentModule.value()) : "";
    }
}

} // namespace Prolog::CodeGen
