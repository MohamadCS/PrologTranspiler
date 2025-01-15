#include "SyntaxChecker.hpp"
#include "ParsingManager.hpp"
#include <memory>

namespace Prolog {

SyntaxChecker::SyntaxChecker(const std::filesystem::path& path)
    : m_targetPath(path),
      m_parsingManager(path) {
}

SyntaxChecker::Status SyntaxChecker::checkSyntax() const {
    auto syntaxErrorListener = SyntaxErrorListener();
    // m_parsingManager.pParser->removeErrorListeners(); 
    //

    m_parsingManager.pParser->addErrorListener(&syntaxErrorListener);
    m_parsingManager.parse(); 
    return syntaxErrorListener.getStatus();
}

} // namespace Prolog
