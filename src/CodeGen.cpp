#include "CodeGen.hpp"
#include "Utils.hpp"
#include <format>
#include <sstream>
#include <vector>

namespace Prolog::CodeGen {

std::string CodeGenVisitor::genVar() {
    return std::format("Var{}", m_varCtr++);
};

std::any CodeGenVisitor::visitTuple(prologParser::TupleContext* ctx) {
    return {};
}

static std::string funcToPredCode(const std::string& predName, const std::vector<std::string>& argsNames,
                                  const std::string& returnVar) {
    std::stringstream argsTuple;
    for (auto& argStr : argsNames) {
        argsTuple << argStr << ",";
    }

    argsTuple << returnVar;

    std::string result = std::format("{} {} :- ", predName, argsTuple.str());
    return result;
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
    visitChildren(ctx);

    // Finish the predicate.
    m_codeBuffer.push_back(".");

    return {};
}

} // namespace Prolog::CodeGen
