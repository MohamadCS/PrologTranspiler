#include "Testing.hpp"
#include "Compiler.hpp"
#include "gtest/gtest.h"
#include <algorithm>
#include <cstddef>
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
                std::cout << std::format("Variable {} is not initialized in function {} \n",
                                         std::forward<decltype(var)>(var), funcsNames[i]);
            };

            std::for_each(setDiff.begin(), setDiff.end(), std::ref(printUninitVars));
        }
    }

    return status;
}

} // namespace Prolog::Testing
