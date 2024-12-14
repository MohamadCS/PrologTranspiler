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

    /**
     * @brief Applies FunctionSemanticsVisitor and returns a pointer to it.
     */
    std::unique_ptr<Prolog::Visitors::FunctionSemanticsVisitor> getFunctionSemanticsData();
    /**
     * @brief Tests if each variable was binded at most once.
     * @return Status::FAIL if some variable was binded more than one time,
     * otherwise Status::SUCCESS.
     */
    Status bindingTest();
    /**
     * @brief Tests if each variable in a function definition was initialized(A
     * function arg or binded).
     * @return Status::FAIL if some variable was not initialized,
     * otherwise Status::SUCCESS.
     */
    Status initTest();
    /**
     * @brief Tests if each function invocation has a function definition somewhere. 
     * @return 
     */
    Status funcDefTest();
    /**
     * @brief Tests if each vanishing statement in a Tuple(Ending with ';') is a binding
     *
     * @return 
     */
    Status vanishingNoBindingTest();

private:
    const std::filesystem::path m_targetPath;
    Prolog::ParsingManager m_parser;
};

std::unique_ptr<SyntaxErrorListener> getSyntaxTestListenerPtr(const std::filesystem::path& path);
}; // namespace Prolog::Testing
