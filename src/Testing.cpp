#include "Testing.hpp"

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

 std::unique_ptr<SyntaxErrorListener> getSyntaxTestListenerPtr(const std::filesystem::path& path) {
    Prolog::Parser parser(path);

    auto syntaxErrorListener = std::make_unique<SyntaxErrorListener>();
    parser.pParser->removeErrorListeners();

    parser.pParser->addErrorListener(syntaxErrorListener.get());
    parser.getStartingRuleNode();

    return syntaxErrorListener;
}
