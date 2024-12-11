#pragma once

#include "Parser.h"
#include "ParserRuleContext.h"
#include "prologBaseVisitor.h"
#include "prologParser.h"
#include "tree/ParseTreeProperty.h"
#include "tree/TerminalNode.h"
#include <cstddef>
#include <cstdint>
#include <list>
#include <set>
#include <variant>

namespace Prolog::Visitors {

struct FunctionSemanticsVisitor : public prologBaseVisitor {
    std::vector<std::set<std::string>> initializedVars;
    std::vector<std::map<std::string, std::size_t>> bindedVars; // Func arg or binded.

    std::set<std::string> functionNames;
    std::set<std::string> functionInvoc;

    std::any visitFunc_def(prologParser::Func_defContext* ctx) override;
    std::any visitBinding(prologParser::BindingContext* ctx) override;
    std::any visitInvoc(prologParser::InvocContext* ctx) override;
    std::any visitTuple(prologParser::TupleContext* ctx) override;
};

// TODO: Delete this class. 
struct VariableSemanticVisitor : public prologBaseVisitor {
    std::any visitVariable(prologParser::VariableContext* ctx) override;
    std::map<std::string, std::uint8_t> varTbl;
    static constexpr std::size_t VAR_COUNT = 2;
};

struct MarkEmptyTuplesVisitor : public prologBaseVisitor {
    antlr4::tree::ParseTreeProperty<bool> emptyTuples;
    std::any visitTuple(prologParser::TupleContext* ctx) override;
};

struct ProgramRestoreVisitor : public prologBaseVisitor {
    std::list<std::list<std::string>> programStmtList = {{}};
    std::optional<antlr4::tree::ParseTreeProperty<bool>> emptyTuples;

    // NOTE: We can use std::variant for a cleaner code for clause/directive
    std::any visitClause(prologParser::ClauseContext* ctx) override;
    std::any visitDirective(prologParser::DirectiveContext* ctx) override;
    std::any visitCompound_term(prologParser::Compound_termContext* ctx) override;
    // NOTE: This method comes form the super class AbstractParseTreeVisitor
    std::any visitTerminal(antlr4::tree::TerminalNode* ctx) override;

    std::any visitTuple(prologParser::TupleContext* ctx) override;
};

} // namespace Prolog::Visitors
