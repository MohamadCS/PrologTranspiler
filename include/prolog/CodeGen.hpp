
#pragma once

#include "BaseStructs.hpp"
#include "prologBaseVisitor.h"
#include "tree/ParseTreeProperty.h"
#include <cstddef>
#include <optional>
#include <string>
#include <vector>

namespace Prolog::CodeGen {

struct Node {
    std::string var;
    bool isEmptyTuple = false;
};

struct CodeGenVisitor : public prologBaseVisitor {
public:
    std::string genVar();
    static std::string genPredName(std::string funcName);

    std::vector<std::string> getCodeBuffer() const;

public:
    std::any visitFunc_def(prologParser::Func_defContext* ctx) override;

    std::any visitBinding(prologParser::BindingContext* ctx) override;

    std::any visitVariable(prologParser::VariableContext* ctx) override;

    std::any visitInvoc(prologParser::InvocContext* ctx) override;

    std::any visitTuple(prologParser::TupleContext* ctx) override;

    std::any visitDirective(prologParser::DirectiveContext* ctx) override;

    std::any visitClause(prologParser::ClauseContext* ctx) override;

    std::any visitExpr(prologParser::ExprContext* ctx) override;

    std::any visitIf(prologParser::IfContext* ctx) override;

    std::any visitIf_else(prologParser::If_elseContext* ctx) override;

    std::any visitBinary_operator(prologParser::Binary_operatorContext* ctx) override;

    std::any visitUnary_operator(prologParser::Unary_operatorContext* ctx) override;

    std::any visitFloat(prologParser::FloatContext* ctx) override;

    std::any visitInteger_term(prologParser::Integer_termContext* ctx) override;

    std::any visitList_term(prologParser::List_termContext* ctx) override;

    std::any visitCompound_term(prologParser::Compound_termContext* ctx) override;

    Node generateArithCode(antlr4::RuleContext* ctx);

private:
    bool m_withinFuncCtx = false;
    std::optional<Prolog::Predicate> m_currentPredicate;
    std::vector<std::string> m_codeBuffer;
    antlr4::tree::ParseTreeProperty<bool> emptyTuples;

    // Function To Predicate name translation
    std::map<std::string, std::string> m_funcToPred;

private:
    static inline std::size_t m_varCtr = 0;
};

} // namespace Prolog::CodeGen
