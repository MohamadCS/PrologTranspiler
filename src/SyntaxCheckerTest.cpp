#include "SyntaxChecker.hpp"
#include "gtest.h"
#include "gtest/gtest.h"

namespace Prolog::Testing {

TEST(FunctionsWithEmptyStmts, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/FunctionsWithEmptyStmts.pl");
    auto syntaxChecker = SyntaxChecker(path);
    EXPECT_EQ(syntaxChecker.checkSyntax(), SyntaxChecker::Status::FAIL);
}

TEST(FunctionsTuples, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/FunctionsTuples.pl");

    auto syntaxChecker = SyntaxChecker(path);
    EXPECT_EQ(syntaxChecker.checkSyntax(), SyntaxChecker::Status::FAIL);
}

TEST(FunctionNoBody, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/FunctionsNoBody.pl");

    auto syntaxChecker = SyntaxChecker(path);
    EXPECT_EQ(syntaxChecker.checkSyntax(), SyntaxChecker::Status::SUCCESS);
}

TEST(FunctionNoArgs, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/FunctionsWithNoArgs.pl");

    auto syntaxChecker = SyntaxChecker(path);
    EXPECT_EQ(syntaxChecker.checkSyntax(), SyntaxChecker::Status::SUCCESS);
}

TEST(FunctionManyArguments, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/FunctionWithManyArguments.pl");

    auto syntaxChecker = SyntaxChecker(path);
    EXPECT_EQ(syntaxChecker.checkSyntax(), SyntaxChecker::Status::SUCCESS);
}

TEST(FunctionsWrongVar, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/FunctionsWrongVar.pl");

    auto syntaxChecker = SyntaxChecker(path);
    EXPECT_EQ(syntaxChecker.checkSyntax(), SyntaxChecker::Status::FAIL);
}

TEST(FunctionsWithLocalStmt, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/FunctionsWithLocalStmt.pl");

    auto syntaxChecker = SyntaxChecker(path);
    EXPECT_EQ(syntaxChecker.checkSyntax(), SyntaxChecker::Status::SUCCESS);
}

TEST(FunctionsLocalStmt, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/FunctionsWithLocalStmt.pl");

    auto syntaxChecker = SyntaxChecker(path);
    EXPECT_EQ(syntaxChecker.checkSyntax(), SyntaxChecker::Status::SUCCESS);
}

TEST(TuplesIssue12, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/TupleIssue12.pl");

    auto syntaxChecker = SyntaxChecker(path);
    EXPECT_EQ(syntaxChecker.checkSyntax(), SyntaxChecker::Status::SUCCESS);
}

TEST(TwoBindings, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/Issue15TwoBinding.pl");

    auto syntaxChecker = SyntaxChecker(path);
    EXPECT_EQ(syntaxChecker.checkSyntax(), SyntaxChecker::Status::SUCCESS);
}
TEST(VanisingNoBinding, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/VanishingNoBinding.pl");

    auto syntaxChecker = SyntaxChecker(path);
    EXPECT_EQ(syntaxChecker.checkSyntax(), SyntaxChecker::Status::SUCCESS);
}

TEST(UninitVars, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/UninitVars.pl");

    auto syntaxChecker = SyntaxChecker(path);
    EXPECT_EQ(syntaxChecker.checkSyntax(), SyntaxChecker::Status::SUCCESS);
}
} // namespace Prolog::Testing
