#include "Visitors.hpp"
#include "ParserRuleContext.h"
#include "Token.h"
#include "Utils.hpp"
#include "prologParser.h"
#include "tree/TerminalNode.h"
#include <cctype>
#include <format>
#include <map>

namespace Prolog::Visitors {

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
    CHECK_NULL(ctx);

    if (markEmptyTuplesV.emptyTuples.get(ctx)) {
        programStmtList.back().push_back("()");
        return {};
    }

    programStmtList.back().push_back("(");

    std::vector<std::string> nonEmptyEntries;
    for (auto* entry : ctx->tuple_entry()) {
        // If its a term, or an non-empty tuple then add it. 
        if (entry->tuple() == nullptr || !markEmptyTuplesV.emptyTuples.get(entry->tuple())) { 
            visit(entry);
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
    for (auto* entry : entryVec) {
        auto* pTuple = entry->tuple();
        if (pTuple != nullptr) { // This is a tuple.
            if (!emptyTuples.get(pTuple)) {
                isEmpty = false;
                break;
            }
        } else{
            isEmpty = false;
            break;
        }
    }

    emptyTuples.put(ctx, isEmpty);

    LOG(std::format("{}: {}", (isEmpty) ? "Empty" : "Not Empty", ctx->getText()));
    return {};
};

} // namespace Prolog::Visitors
