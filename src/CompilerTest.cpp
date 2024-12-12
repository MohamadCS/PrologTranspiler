#include "Testing.hpp"
#include "gtest.h"
#include "gtest/gtest.h"

using namespace Prolog::Testing;

TEST(FunctionsWithEmptyStmts, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/FunctionsWithEmptyStmts.pl");
    auto syntaxErrorListener = getSyntaxTestListenerPtr(path);
    EXPECT_EQ(syntaxErrorListener->getStatus(), Status::FAIL);
}

TEST(FunctionsTuples, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/FunctionsTuples.pl");
    auto syntaxErrorListener = getSyntaxTestListenerPtr(path);
    EXPECT_EQ(syntaxErrorListener->getStatus(), Status::FAIL);
}

TEST(FunctionNoBody, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/FunctionsNoBody.pl");
    auto syntaxErrorListener = getSyntaxTestListenerPtr(path);
    EXPECT_EQ(syntaxErrorListener->getStatus(), Status::SUCCESS);
}

TEST(FunctionNoArgs, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/FunctionsWithNoArgs.pl");
    auto syntaxErrorListener = getSyntaxTestListenerPtr(path);
    EXPECT_EQ(syntaxErrorListener->getStatus(), Status::SUCCESS);
}

TEST(FunctionManyArguments, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/FunctionWithManyArguments.pl");
    auto syntaxErrorListener = getSyntaxTestListenerPtr(path);
    EXPECT_EQ(syntaxErrorListener->getStatus(), Status::SUCCESS);
}

TEST(FunctionsWrongVar, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/FunctionsWrongVar.pl");
    auto syntaxErrorListener = getSyntaxTestListenerPtr(path);
    EXPECT_EQ(syntaxErrorListener->getStatus(), Status::FAIL);
}

TEST(FunctionsWithLocalStmt, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/FunctionsWithLocalStmt.pl");
    auto syntaxErrorListener = getSyntaxTestListenerPtr(path);
    EXPECT_EQ(syntaxErrorListener->getStatus(), Status::SUCCESS);
}

TEST(FunctionsLocalStmt, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/FunctionsWithLocalStmt.pl");
    auto syntaxErrorListener = getSyntaxTestListenerPtr(path);
    EXPECT_EQ(syntaxErrorListener->getStatus(), Status::SUCCESS);
}

TEST(TuplesIssue12, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/TupleIssue12.pl");
    auto syntaxErrorListener = getSyntaxTestListenerPtr(path);
    EXPECT_EQ(syntaxErrorListener->getStatus(), Status::SUCCESS);
}


TEST(TwoBindings, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/Issue15TwoBinding.pl");
    auto syntaxErrorListener = getSyntaxTestListenerPtr(path);
    EXPECT_EQ(syntaxErrorListener->getStatus(), Status::SUCCESS);
}

TEST(TwoBindings, SemanticsTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/Issue15TwoBinding.pl");
    SemanticsTest semanticsTest(path);
    EXPECT_EQ(semanticsTest.bindingTest(), Status::FAIL);
}

TEST(UninitVars, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/UninitVars.pl");
    auto syntaxErrorListener = getSyntaxTestListenerPtr(path);
    EXPECT_EQ(syntaxErrorListener->getStatus(), Status::SUCCESS);
}

TEST(UninitVars, SemanticsTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/UninitVars.pl");
    SemanticsTest semanticsTest(path);
    EXPECT_EQ(semanticsTest.initTest(), Status::FAIL);
}


TEST(VanisingNoBinding, SyntaxTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/VanishingNoBinding.pl");
    auto syntaxErrorListener = getSyntaxTestListenerPtr(path);
    EXPECT_EQ(syntaxErrorListener->getStatus(), Status::SUCCESS);
}

TEST(VanisingNoBinding, SemanticsTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/VanishingNoBinding.pl");
    SemanticsTest semanticsTest(path);

    EXPECT_EQ(semanticsTest.initTest(), Status::FAIL);
    EXPECT_EQ(semanticsTest.funcDefTest(), Status::SUCCESS);
    EXPECT_EQ(semanticsTest.bindingTest(), Status::SUCCESS);
    EXPECT_EQ(semanticsTest.vanishingNoBindingTest(), Status::FAIL);
}
