#include "BaseErrorListener.h"
#include "Compiler.hpp"
#include "Visitors.hpp"
#include <filesystem>
#include <memory>

namespace Prolog::Testing {
enum class Status {
    ERROR = 0,
    FAIL,
    SUCCESS
};

class SyntaxErrorListener : public antlr4::BaseErrorListener {
public:
    Status getStatus() const {
        return m_testStatus;
    }

private:
    Status m_testStatus = Status::SUCCESS;
    void syntaxError(antlr4::Recognizer* recognizer, antlr4::Token* offendingSymbol, size_t line,
                     size_t charPositionInLine, const std::string& msg, std::exception_ptr e) override {
        m_testStatus = Status::FAIL;
    };
};

class SemanticsTest {
public:
    SemanticsTest(const std::filesystem::path& path);
    std::unique_ptr<Prolog::Visitors::FunctionSemanticsVisitor> getFunctionSemanticsData();
    Status bindingTest();
    Status initTest();
    Status funcDefTest();

private:
    const std::filesystem::path m_targetPath;
    Prolog::ParsingManager m_parser;
};

std::unique_ptr<SyntaxErrorListener> getSyntaxTestListenerPtr(const std::filesystem::path& path);
}; // namespace Prolog::Testing
