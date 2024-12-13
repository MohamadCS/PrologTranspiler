#include "Visitors.hpp"
#include "Token.h"
#include "Utils.hpp"
#include "prologParser.h"
#include "support/CPPUtils.h"
#include "tree/TerminalNode.h"
#include <cassert>
#include <cctype>
#include <deque>
#include <format>
#include <map>
#include <unistd.h>

namespace Prolog::Visitors {
static bool entryIsTuple(prologParser::Tuple_entryContext* ctx);

std::any ProgramRestoreVisitor::visitClause(prologParser::ClauseContext* ctx) {
    CHECK_NULL(ctx);

    programStmtList.push_back({});

    return visitChildren(ctx);
}

std::any ProgramRestoreVisitor::visitDirective(prologParser::DirectiveContext* ctx) {
    CHECK_NULL(ctx);

    programStmtList.push_back({});

    return visitChildren(ctx);
}

std::any ProgramRestoreVisitor::visitCompound_term(prologParser::Compound_termContext* ctx) {
    CHECK_NULL(ctx);

    programStmtList.back().push_back(ctx->getText());
    return {};
}
std::any ProgramRestoreVisitor::visitTerminal(antlr4::tree::TerminalNode* ctx) {
    CHECK_NULL(ctx);

    if (ctx->getSymbol()->getType() != antlr4::Token::EOF) {
        programStmtList.back().push_back(ctx->getText());
    }

    return visitChildren(ctx);
}

std::any ProgramRestoreVisitor::visitTuple(prologParser::TupleContext* ctx) {

    if (!emptyTuples.has_value()) {
        return visitChildren(ctx);
    }

    CHECK_NULL(ctx);

    if (emptyTuples.value().get(ctx)) {
        programStmtList.back().push_back("()");
        return {};
    }

    programStmtList.back().push_back("(");

    std::vector<std::string> nonEmptyEntries;
    for (auto* pEntry : ctx->tuple_entry()) {
        // If its a term, or an non-empty tuple then add it.
        if (!entryIsTuple(pEntry) || !emptyTuples.value().get(pEntry->expr()->tuple())) {
            visit(pEntry);
            programStmtList.back().push_back(",");
        }
    }

    if (programStmtList.back().back() == ",") {
        programStmtList.back().pop_back();
    }

    programStmtList.back().push_back(")");
    return {};
};

std::any VariableSemanticVisitor::visitVariable(prologParser::VariableContext* ctx) {
    CHECK_NULL(ctx);

    const std::string& varName = ctx->getText();

    if (varName == "_") {
        return visitChildren(ctx);
    }

    if (auto it = varTbl.find(varName); it != varTbl.end()) {
        auto& [_, count] = *it;
        ++count;
    } else {
        varTbl.insert({varName, 1});
    }

    return visitChildren(ctx);
}

std::any MarkEmptyTuplesVisitor::visitTuple(prologParser::TupleContext* ctx) {
    CHECK_NULL(ctx);

    // postorder
    visitChildren(ctx);

    auto entryVec = ctx->tuple_entry();

    bool isEmpty = true; // Base: if there are no entries then the for won't do any iteration.
    for (auto* pEntry : entryVec) {
        if (entryIsTuple(pEntry)) { // This is a tuple.
            if (!emptyTuples.get(pEntry->expr()->tuple())) {
                isEmpty = false;
                break;
            }
        } else {
            isEmpty = false;
            break;
        }
    }

    emptyTuples.put(ctx, isEmpty);
    return {};
};

std::any FunctionSemanticsVisitor::visitFunc_def(prologParser::Func_defContext* ctx) {
    CHECK_NULL(ctx);

    const std::string& funcName = ctx->VARIABLE()->getText();

    bindedVars.push_back({});
    initializedVars.push_back({});
    funcsVars.push_back({});
    ;

    functionNames.push_back(funcName);

    const auto& argsVec = ctx->func_args()->VARIABLE();
    // Every argument variable is initialized.
    for (auto* pFuncArg : argsVec) {
        initializedVars.back().insert(pFuncArg->getText());
        funcsVars.back().insert(pFuncArg->getText());
    }
    withinFuncCtx = true;
    visitChildren(ctx);
    withinFuncCtx = false;
    return {};
}

std::any FunctionSemanticsVisitor::visitBinding(prologParser::BindingContext* ctx) {
    CHECK_NULL(ctx);

    const auto* targetVar = ctx->VARIABLE();
    CHECK_NULL(targetVar);

    const std::string& varName = targetVar->getSymbol()->getText();

    // First binding
    if (auto it = bindedVars.back().find(varName); it != bindedVars.back().end()) {
        auto& [_, count] = *it;
        count++;
    } else /* Was binded before*/ { // We can print an error, better to just record it then check in unit tests.
        bindedVars.back().insert({varName, 1});
    }

    // Var is initialized
    if (auto it = initializedVars.back().find(varName); it == initializedVars.back().end()) {
        initializedVars.back().insert(varName);
    }

    return visitChildren(ctx);
}

std::any FunctionSemanticsVisitor::visitInvoc(prologParser::InvocContext* ctx) {
    CHECK_NULL(ctx);

    functionInvoc.insert(ctx->VARIABLE()->getText());

    return visitChildren(ctx);
}

std::any FunctionSemanticsVisitor::visitTuple(prologParser::TupleContext* ctx) {
    CHECK_NULL(ctx);

    // Divide into vanishing and non vanishing rules.

    if (ctx->tuple_entry().empty()) {
        return visitChildren(ctx);
    }

    std::stack<prologParser::Tuple_entryContext*> vanishingRules;

    const auto& tupleEntriesVec = ctx->tuple_entry();

    std::deque<bool> isVanishing;
    for (auto& child : ctx->children) {
        if (child->getText() == ";") {
            isVanishing.push_back(true);
        }
        if (child->getText() == ",") {
            isVanishing.push_back(false);
        }
    }
    // Last element does not specify ',' or ';' which means its non-vanishing

    if (isVanishing.size() < tupleEntriesVec.size()) {
        isVanishing.push_back(false);
    }

    for (int i = 0; i < tupleEntriesVec.size(); ++i) {
        auto* pEntry = tupleEntriesVec[i];
        // LOG(std::format("Tuple Entry: {} is {}", pEntry->getText(),(isVanishing[i] ? "vanishing" : "non-vanishing")));
        if (isVanishing[i]) {
            if (pEntry->expr() != nullptr) { // Meaning its a term
                vanishingNoBinding.push_back(pEntry->expr());
            }
        }
    }

    return visitChildren(ctx);
}

std::any FunctionSemanticsVisitor::visitVariable(prologParser::VariableContext* ctx) {
    CHECK_NULL(ctx);

    if (withinFuncCtx) {
        funcsVars.back().insert(ctx->getText());
    }

    return visitChildren(ctx);
}

/**** Static Functions Implementations ****/

static bool entryIsTuple(prologParser::Tuple_entryContext* ctx) {
    CHECK_NULL(ctx);
    if (ctx->expr() != nullptr && ctx->expr()->tuple() != nullptr) {
        return true;
    }
    return false;
}

} // namespace Prolog::Visitors
