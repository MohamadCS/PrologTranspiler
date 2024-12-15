#pragma once

#include "Parser.h"
#include "ParserRuleContext.h"
#include "RuleContext.h"
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

// NOTE: Any functions that begins with "visit" is an ANTLR generated function.

/**
 * @class FunctionSemanticsVisitor
 * @brief Provides data about the functions semantics.
 *
 */
struct FunctionSemanticsVisitor : public prologBaseVisitor {

    /**
     * @brief A vector that stores the initialized variabls.
     * An initialized variable, is an argument of the function, or a binded variable.
     */
    std::vector<std::set<std::string>> initializedVars;

    /**
     * @brief Each entry in the vector is a for a different function, containing a
     *  set of all variables that appear in the function.
     */
    std::vector<std::set<std::string>> funcsVars; // Func arg or binded.

    /**
     * @brief Each entry in the vector is a for a different function, containing a map
     * that maps a variable to the number of bindings it has.
     */
    std::vector<std::map<std::string, std::size_t>> bindedVars; // Func arg or binded.

    /**
     * @brief A set of the function names.
     */
    std::vector<std::string> functionNames;

    /**
     * @brief A set of invoked function, even if the function is not defined.
     */
    std::set<std::string> functionInvoc;

    /**
     * @brief A list of expr's within a vanishing entry context.
     */
    std::list<antlr4::ParserRuleContext*> vanishingNoBinding;

    std::any visitFunc_def(prologParser::Func_defContext* ctx) override;
    std::any visitBinding(prologParser::BindingContext* ctx) override;
    std::any visitInvoc(prologParser::InvocContext* ctx) override;
    std::any visitTuple(prologParser::TupleContext* ctx) override;
    std::any visitVariable(prologParser::VariableContext* ctx) override;

private:
    bool m_withinFuncCtx = false;
};

struct MarkEmptyTuplesVisitor : public prologBaseVisitor {
    /**
     * @brief A property that gives wither a tuple is empty or not.
     * An empty tuple is '()', or a tuple of empty tuples.
     */
    antlr4::tree::ParseTreeProperty<bool> emptyTuples;

    std::any visitTuple(prologParser::TupleContext* ctx) override;
};

struct ProgramRestoreVisitor : public prologBaseVisitor {
    /**
     * @brief Each element of the list represents a stmt
     * A stmt is a clause | directive | func_def
     */
    std::list<std::list<std::string>> programStmtList = {{}};
    /**
     * @brief This is an optional property, if it is not nullopt
     * then the empty tuples will be reduced.
     */
    std::optional<antlr4::tree::ParseTreeProperty<bool>> emptyTuples;

    // NOTE: We can use std::variant for a cleaner code for clause/directive
    std::any visitClause(prologParser::ClauseContext* ctx) override;
    std::any visitDirective(prologParser::DirectiveContext* ctx) override;
    std::any visitCompound_term(prologParser::Compound_termContext* ctx) override;
    // NOTE: This method comes form the super class AbstractParseTreeVisitor
    std::any visitTerminal(antlr4::tree::TerminalNode* ctx) override;

    std::any visitTuple(prologParser::TupleContext* ctx) override;
};

// TODO: Delete this class.
// struct VariableSemanticVisitor : public prologBaseVisitor {
//     std::any visitVariable(prologParser::VariableContext* ctx) override;
//
//     std::map<std::string, std::uint8_t> varTbl;
//     static constexpr std::size_t VAR_COUNT = 2;
// };
//
} // namespace Prolog::Visitors
