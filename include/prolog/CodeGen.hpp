
#pragma once

#include "BaseStructs.hpp"
#include "prologBaseVisitor.h"
#include "tree/ParseTreeProperty.h"
#include <cstddef>
#include <deque>
#include <optional>
#include <stack>
#include <string>
#include <vector>

namespace Prolog::CodeGen {

struct Node {
    std::string var;
    bool isEmptyTuple = false;
    bool isPredicate = false;
    std::string predicateText;
};

class CodeGenVisitor : public prologBaseVisitor {
public:
    explicit CodeGenVisitor(bool formatOutput = false)
        : prologBaseVisitor(),
          m_formatOutput(formatOutput) {
    }

    std::string genVar();

    std::vector<std::string> getCodeBuffer() const;

    void setFuncNames(const std::vector<std::string>& funcNames);

public:
    std::any visitImport_modules(prologParser::Import_modulesContext* ctx) override;

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

    std::any visitName(prologParser::NameContext* ctx) override;

    std::any visitLambda(prologParser::LambdaContext* ctx) override;

    std::any visitModule(prologParser::ModuleContext* ctx) override;

    std::any visitAtom_term(prologParser::Atom_termContext* ctx) override;

    std::any visitReturn(prologParser::ReturnContext* ctx) override;

    std::any visitType_def(prologParser::Type_defContext* ctx) override;

    std::any visitMatch_stmt(prologParser::Match_stmtContext* ctx) override;


    Node generateArithCode(antlr4::RuleContext* ctx);

private:
    bool m_insideLambda = false;
    bool m_formatOutput = false;
    std::size_t m_currentTabs = 0;
    std::optional<Prolog::Predicate> m_currentPredicate;

    std::stack<Prolog::Predicate> m_predicateStack;

    std::set<std::string> m_funcNames;
    std::vector<std::string> m_codeBuffer;
    std::vector<std::string> m_lambdasBuffer;
    std::vector<std::string> m_importedModules;

    std::set<std::string> m_lambdasNames;

    std::map<std::string, std::deque<Predicate>> m_modules;
    std::optional<std::string> m_currentModule;

    static inline std::size_t m_varCtr = 0;
    static inline std::size_t m_lambdaCtr = 0;

private:
    static std::string genPredName(std::string funcName);
    void emit(std::string&&);
    std::string getModulesCode() const;
    std::string getNameSpace(prologParser::InvocContext*) const;

    std::string genTypeCode(std::string varName, prologParser::TypeContext* pTypeCtx);
};

} // namespace Prolog::CodeGen
