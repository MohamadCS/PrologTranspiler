#include "CodeGen.hpp"
#include "Utils.hpp"
#include <any>
#include <cctype>
#include <format>
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

std::any CodeGenVisitor::visitBinding(prologParser::BindingContext* ctx) {
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
    LOG(predicateInvoc);
    m_codeBuffer.push_back(predicateInvoc);

    return invocNode;
}

std::any CodeGenVisitor::visitTuple(prologParser::TupleContext* ctx) {
    CHECK_NULL(ctx);

    const auto isVanishingList = Utility::isVanishingEntryList(ctx);

    // Add non-vanishing results to the tuple evaluation

    int i = 0;
    std::vector<std::string> varValuesVec;
    for (auto* child : ctx->tuple_entry()) {
        Node value = std::any_cast<Node>(visit(child));
        if (!isVanishingList[i++]) {
            varValuesVec.push_back(value.var);
        }
    }

    Node tupleNode;

    tupleNode.var = genVar();

    auto itemFormatFunc = [](decltype(varValuesVec.begin()) it) { return *it; };
    m_codeBuffer.push_back(std::format("{} = tpl( {} ),", tupleNode.var,
                                       Prolog::Utility::convertContainerToListStr<decltype(varValuesVec.begin())>(
                                           varValuesVec.begin(), varValuesVec.end(), itemFormatFunc)));

    return tupleNode;
}

std::any CodeGenVisitor::visitFunc_def(prologParser::Func_defContext* ctx) {
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

    Node node;

    node.var = ctx->getText();
    return node;
}

std::any CodeGenVisitor::visitExpr(prologParser::ExprContext* ctx) {
    CHECK_NULL(ctx);
    // NewVar = Expr,
    //

    Node node;

    node.var = genVar();

    std::string exprStr = ctx->getText();

    std::any value = visit(ctx->children[0]);

    if(value.has_value()){
        Node resultNode = std::any_cast<Node>(value);
        std::string result = std::format("{} = {},", node.var, resultNode.var);
        m_codeBuffer.push_back(result);
    }
    else  {
        m_codeBuffer.push_back(std::format("{},", exprStr));
        node.isTerm = true;
    }

    return node;
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
