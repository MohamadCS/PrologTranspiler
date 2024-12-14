#include "SemanticChecker.hpp"
#include "gtest.h"

namespace Prolog::Testing {

static void testFuncVisitorValidity(const Visitors::FunctionSemanticsVisitor& funcV){
    auto& bindedVars = funcV.bindedVars;
    auto& funcsNames = funcV.functionNames;
    auto& funcsVars = funcV.funcsVars;
    auto& initVars = funcV.initializedVars;

    std::size_t funcsNum = funcsNames.size();

    EXPECT_EQ(funcsVars.size(), initVars.size());
    EXPECT_EQ(funcsVars.size(), funcsNames.size());
    EXPECT_EQ(bindedVars.size(), funcsNames.size());

    for (std::size_t i = 0; i < funcsNum; ++i) {
        EXPECT_LE(initVars[i].size(), funcsVars[i].size()); // The set of init vars is always less than the total vars
    }
}

TEST(TwoBindings, SemanticsTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/Issue15TwoBinding.pl");
    SemanticChecker semanticChecker(path);
    testFuncVisitorValidity(semanticChecker.getFuncVisitor());
    EXPECT_EQ(semanticChecker.checkUniqueBinding(), SemanticChecker::Status::FAIL);
}

TEST(UninitVars, SemanticsTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/UninitVars.pl");
    SemanticChecker semanticChecker(path);
    testFuncVisitorValidity(semanticChecker.getFuncVisitor());

    EXPECT_EQ(semanticChecker.checkFuncInitVariables(), SemanticChecker::Status::FAIL);
}



TEST(VanisingNoBinding, SemanticsTest) {
    std::filesystem::path path = std::filesystem::current_path() / ("tests/VanishingNoBinding.pl");
    SemanticChecker semanticChecker(path);
    testFuncVisitorValidity(semanticChecker.getFuncVisitor());

    EXPECT_EQ(semanticChecker.checkFuncInitVariables(), SemanticChecker::Status::FAIL);
    EXPECT_EQ(semanticChecker.checkInvocImpliesDefine(), SemanticChecker::Status::SUCCESS);
    EXPECT_EQ(semanticChecker.checkUniqueBinding(), SemanticChecker::Status::SUCCESS);
    EXPECT_EQ(semanticChecker.checkVanishingImpliesBinding(), SemanticChecker::Status::FAIL);
}


}; // namespace Prolog::Testing
