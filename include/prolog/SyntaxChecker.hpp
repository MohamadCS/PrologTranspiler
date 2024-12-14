#pragma once

#include "BaseErrorListener.h"
#include "ParsingManager.hpp"
#include <filesystem>
#include <memory>

namespace Prolog {

class SyntaxChecker {
public:
    enum class Status {
        ERROR = 0,
        FAIL,
        SUCCESS
    };

    SyntaxChecker(const std::filesystem::path&);

    Status checkSyntax() const;

private:
    std::filesystem::path m_targetPath;
    ParsingManager m_parsingManager;
};

class SyntaxErrorListener : public antlr4::BaseErrorListener {
public:
    SyntaxChecker::Status getStatus() const {
        return m_testStatus;
    }

private:
    SyntaxChecker::Status m_testStatus = SyntaxChecker::Status::SUCCESS;
    void syntaxError(antlr4::Recognizer* recognizer, antlr4::Token* offendingSymbol, size_t line,
                     size_t charPositionInLine, const std::string& msg, std::exception_ptr e) override {
        m_testStatus = SyntaxChecker::Status::FAIL;
    };
};

// std::unique_ptr<SyntaxErrorListener> getSyntaxTestListenerPtr(const std::filesystem::path& path);
}; // namespace Prolog
