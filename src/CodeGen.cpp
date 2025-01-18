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

std::vector<std::string> CodeGenVisitor::getCodeBuffer() const {
    return m_codeBuffer;
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
    return std::format("Var{}", m_varCtr++);
};

static std::string funcToPredCode(const std::string& predName, const std::vector<std::string>& argsNames,
                                  const std::string& returnVar) {
    std::stringstream argsTuple;
    for (auto& argStr : argsNames) {
        argsTuple << argStr << ",";
    }

    argsTuple << returnVar;

    std::string result = std::format("{}({}) :- ", predName, argsTuple.str());
    return result;
}

std::any CodeGenVisitor::visitIf(prologParser::IfContext* ctx) {
    LOG("If");
    CHECK_NULL(ctx);

    auto* conditionTerm = ctx->term();
    m_codeBuffer.push_back(std::format("( ({} -> (", conditionTerm->getText()));

    std::any result = visit(ctx->tuple());
    m_codeBuffer.push_back("true);true)),");

    return result;
}

std::any CodeGenVisitor::visitIf_else(prologParser::If_elseContext* ctx) {
    LOG("If else");
    CHECK_NULL(ctx);

    Node node;
    node.var = genVar();

    auto* conditionTerm = ctx->term();
    m_codeBuffer.push_back(std::format("({} -> (", conditionTerm->getText()));

    auto* ifBodyTuple = ctx->tuple()[0];
    auto* elseBodyTuple = ctx->tuple()[1];

    std::any returnValue;

    Node ifRetValue = std::any_cast<Node>(visit(ifBodyTuple));
    m_codeBuffer.push_back(std::format("{} = {})",node.var,ifRetValue.var));

    m_codeBuffer.push_back(";(");

    Node elseRetValue = std::any_cast<Node>(visit(elseBodyTuple));
    m_codeBuffer.push_back(std::format("{} = {})",node.var,elseRetValue.var));

    m_codeBuffer.push_back("),");

    return node;
}

std::any CodeGenVisitor::visitBinding(prologParser::BindingContext* ctx) {
    LOG("Binding");
    CHECK_NULL(ctx);

    Node node = std::any_cast<Node>(visit(ctx->expr()));
    std::string bindedVarName = ctx->VARIABLE()->getText();

    std::string result = std::format("{} = {},", bindedVarName, node.var);

    m_codeBuffer.push_back(std::move(result));

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

    // Evaluate arguments, and add them to the vector.
    std::vector<std::string> predicateArgs;
    for (auto* child : tuple->tuple_entry()) {
        Node node = std::any_cast<Node>(visit(child));
        predicateArgs.push_back(node.var);
    }

    Node invocNode;

    invocNode.var = genVar();
    predicateArgs.push_back(invocNode.var); // Result var

    std::string predicateName = m_funcToPred.at(funcName);

    auto itemFormatFunc = [](decltype(predicateArgs.begin()) it) { return *it; };
    std::string predicateInvoc =
        std::format("{}({}),", predicateName,
                    Prolog::Utility::convertContainerToListStr<decltype(predicateArgs.begin())>(
                        predicateArgs.begin(), predicateArgs.end(), itemFormatFunc));
    m_codeBuffer.push_back(predicateInvoc);

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
        if (!isVanishingList[i++]) {
            Node value = std::any_cast<Node>(returnValue);
            varValuesVec.push_back(value.var);
        }
    }

    Node tupleNode;

    tupleNode.var = genVar();

    auto itemFormatFunc = [](decltype(varValuesVec.begin()) it) { return *it; };

    std::string resultStr;

    // If the tuple is empty then dont store it in a variable, and return an empty node.

    if (varValuesVec.size() == 1) {
        resultStr = std::format("{} = {},", tupleNode.var, varValuesVec[0]);
    } else {
        resultStr = std::format("{} = tuple( {} ),", tupleNode.var,
                                Prolog::Utility::convertContainerToListStr<decltype(varValuesVec.begin())>(
                                    varValuesVec.begin(), varValuesVec.end(), itemFormatFunc));
    }

    m_codeBuffer.push_back(resultStr);

    LOG("END TUPLE");
    return tupleNode;
}

std::any CodeGenVisitor::visitFunc_def(prologParser::Func_defContext* ctx) {
    LOG("Func def");
    CHECK_NULL(ctx);

    m_withinFuncCtx = true;
    const std::string funcName = ctx->VARIABLE()->getText();

    /*Preparing the predicate*/
    Predicate currentPredicate;

    currentPredicate.name = genPredName(funcName);
    m_funcToPred.insert({funcName, currentPredicate.name});

    auto argsVec = ctx->func_args()->VARIABLE();
    for (auto* pVarCtx : argsVec) {
        currentPredicate.args.push_back(pVarCtx->getText());
    }
    currentPredicate.returnVar = genVar();

    m_currentPredicate = std::move(currentPredicate);
    /****/

    // Emit func(args) :-
    m_codeBuffer.push_back(
        funcToPredCode(m_currentPredicate->name, m_currentPredicate->args, m_currentPredicate->returnVar));
    m_withinFuncCtx = false;

    // Fill the predicate body.
    Node result = std::any_cast<Node>(visit(ctx->tuple()));

    std::string resultStmt = std::format("{} = {}", m_currentPredicate->returnVar, result.var);

    m_codeBuffer.push_back(resultStmt);

    // Finish the predicate.
    m_codeBuffer.push_back(".");

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

    m_codeBuffer.push_back(std::format("{} is {},", node.var, arithExp));

    return node;
}

Node CodeGenVisitor::generateArithCode(antlr4::RuleContext* ctx) {
    Node node;

    node.var = genVar();

    std::string arithExp = ctx->getText();

    m_codeBuffer.push_back(std::format("{} is {},", node.var, arithExp));

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
        m_codeBuffer.push_back(std::format("{},", ctx->getText()));
        return {};
    }
}

std::any CodeGenVisitor::visitDirective(prologParser::DirectiveContext* ctx) {
    CHECK_NULL(ctx);

    m_codeBuffer.push_back(ctx->getText());
    return {};
}

std::any CodeGenVisitor::visitClause(prologParser::ClauseContext* ctx) {
    CHECK_NULL(ctx);
    m_codeBuffer.push_back(ctx->getText());
    return {};
}

} // namespace Prolog::CodeGen
