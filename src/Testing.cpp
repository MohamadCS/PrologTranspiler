#include "Testing.hpp"
#include "Compiler.hpp"
#include "gtest/gtest.h"
#include <algorithm>
#include <cstddef>
#include <format>
#include <functional>
#include <iterator>

namespace Prolog::Testing {

std::unique_ptr<SyntaxErrorListener> getSyntaxTestListenerPtr(const std::filesystem::path& path) {
    Prolog::ParsingManager parsingManager(path);

    auto syntaxErrorListener = std::make_unique<SyntaxErrorListener>();
    parsingManager.pParser->removeErrorListeners();

    parsingManager.pParser->addErrorListener(syntaxErrorListener.get());
    parsingManager.getStartingRuleNode();

    return syntaxErrorListener;
}

SemanticsTest::SemanticsTest(const std::filesystem::path& targetPath)
    : m_targetPath(targetPath),
      m_parser(targetPath) {
}

std::unique_ptr<Prolog::Visitors::FunctionSemanticsVisitor> SemanticsTest::getFunctionSemanticsData() {
    m_parser.pParser->reset();
    auto pFuncV = std::make_unique<Prolog::Visitors::FunctionSemanticsVisitor>();
    auto* pStartCtx = m_parser.getStartingRuleNode();
    pFuncV->visit(pStartCtx);
    return pFuncV;
}

Status SemanticsTest::initTest() {
    auto pFuncV = getFunctionSemanticsData();
    auto& funcsVars = pFuncV->funcsVars;
    auto& initVars = pFuncV->initializedVars;
    auto& funcsNames = pFuncV->functionNames;

    std::size_t funcsNum = initVars.size();
    EXPECT_EQ(funcsVars.size(), initVars.size());
    EXPECT_EQ(funcsVars.size(), funcsNames.size());

    Status status = Status::SUCCESS;

    for (std::size_t i = 0; i < funcsNum; ++i) {
        std::set<std::string> setDiff;

        EXPECT_LE(initVars[i].size(), funcsVars[i].size()); // The set of init vars is always less than the total vars

        std::set_difference(funcsVars[i].begin(), funcsVars[i].end(), initVars[i].begin(), initVars[i].end(),
                            std::inserter(setDiff, setDiff.begin()));

        if (!setDiff.empty()) {
            status = Status::FAIL;
            auto printUninitVars = [i, &funcsNames](auto&& var) {
                std::cerr << std::format("Variable {} is not initialized in function {} \n",
                                         std::forward<decltype(var)>(var), funcsNames[i]);
            };

            std::for_each(setDiff.begin(), setDiff.end(), std::ref(printUninitVars));
        }
    }

    return status;
}

Status SemanticsTest::bindingTest() {
    auto pFuncV = getFunctionSemanticsData();

    auto& bindedVars = pFuncV->bindedVars;
    auto& funcsNames = pFuncV->functionNames;

    EXPECT_EQ(bindedVars.size(), funcsNames.size());

    auto status = Status::SUCCESS;
    for (std::size_t i = 0; i < bindedVars.size(); ++i) {
        for (const auto& [varName, count] : bindedVars[i]) {
            if (count > 1) {
                std::cerr << std::format("ERROR: Variable {} in function {} has more than one binding({}).\n", varName,
                                         funcsNames[i], count);
                status = Status::FAIL;
            }
        }
    }

    return status;
}

Status SemanticsTest::funcDefTest() {
    auto pFuncV = getFunctionSemanticsData();

    auto funcNamesSet = std::set<std::string>(pFuncV->functionNames.begin(), pFuncV->functionNames.end());
    auto& funcsInvockSet = pFuncV->functionInvoc;

    std::set<std::string> setDiff;

    // Compute funcInvokeSet / funcNamesSet, if its an empty set, then all funcInvokes are legal. 
    std::set_difference(funcsInvockSet.begin(),funcsInvockSet.end(),funcNamesSet.begin(),funcNamesSet.end(),std::inserter(setDiff, setDiff.end()));

    if(setDiff.empty()){
        return Status::SUCCESS;
    }

    for(auto& funcName : setDiff){
        std::cerr << std::format("ERROR: Function {} is invoked but not defined.\n", funcName); 
    }
    return Status::FAIL;
}

Status SemanticsTest::vanishingNoBindingTest(){
    auto pFuncV = getFunctionSemanticsData();

    if(pFuncV->vanishingNoBinding.empty()){
        return Status::SUCCESS;
    }

    for(auto& bindingNode : pFuncV->vanishingNoBinding ){
        std::cout << std::format("ERROR(Useless Code): Vanishing statement {} is not a binding\n", bindingNode->getText());
    }

    return Status::FAIL;
}

} // namespace Prolog::Testing
