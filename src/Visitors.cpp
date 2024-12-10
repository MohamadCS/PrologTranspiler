#include "Visitors.hpp"
#include "Token.h"
#include "Utils.hpp"
#include "prologParser.h"
#include "tree/TerminalNode.h"
#include <cctype>
#include <format>
#include <map>

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

static bool entryIsTuple(prologParser::Tuple_entryContext* ctx) {
    CHECK_NULL(ctx);
    if (ctx->expr() != nullptr && ctx->expr()->tuple() != nullptr) {
        return true;
    }
    return false;
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

    LOG(std::format("{}: {}", (isEmpty) ? "Empty" : "Not Empty", ctx->getText()));
    return {};
};

} // namespace Prolog::Visitors
