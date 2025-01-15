
#pragma once

#include "BaseStructs.hpp"
#include <cstddef>
#include <optional>
#include <prologBaseVisitor.h>
#include <string>
#include <vector>

namespace Prolog::CodeGen {

struct Node {
    std::string var;
    bool isTerm = false;
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


private:
    bool m_withinFuncCtx = false;
    std::optional<Prolog::Predicate> m_currentPredicate;
    std::vector<std::string> m_codeBuffer;

    // Function To Predicate name translation
    std::map<std::string, std::string> m_funcToPred;

private:
    static inline std::size_t m_varCtr = 0;
};

} // namespace Prolog::CodeGen
