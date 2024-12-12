
#include "Compiler.hpp"
#include "BaseErrorListener.h"
#include "Utils.hpp"
#include "Visitors.hpp"
#include "gtest.h"
#include "prologLexer.h"
#include "prologParser.h"
#include <filesystem>
#include <memory>

class SyntaxErrorListener : public antlr4::BaseErrorListener {
public:
    bool isError() const {
        return m_syntaxError;
    }

private:
    bool m_syntaxError = false;
    void syntaxError(antlr4::Recognizer* recognizer, antlr4::Token* offendingSymbol, size_t line,
                     size_t charPositionInLine, const std::string& msg, std::exception_ptr e) override {
        m_syntaxError = true;
    };
};

class SemanticsTest {
public:
    SemanticsTest(const std::filesystem::path& path);
    std::unique_ptr<Prolog::Visitors::FunctionSemanticsVisitor> getFunctionSemanticsData();

private:
    const std::filesystem::path m_targetPath;
    Prolog::Parser m_parser;
};

std::unique_ptr<SyntaxErrorListener> getSyntaxTestListenerPtr(const std::filesystem::path& path) ;
