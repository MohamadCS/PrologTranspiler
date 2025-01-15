#include "ParsingManager.hpp"

namespace Prolog {

antlr4::tree::ParseTree* ParsingManager::getStartingRuleNode() const {
    pParser->reset();
    return pParser->p_text();
}
ParsingManager::ParsingManager(const std::filesystem::path& path)
    : targetPath(path) {
    std::ifstream targetFile{path};
    if (!targetFile) {
        std::cerr << std::format("ERROR: can't open the file {}\n", path.string());
    }
    pInputStream = std::make_unique<antlr4::ANTLRInputStream>(targetFile);
    pLexer = std::make_unique<prologLexer>(pInputStream.get());
    pTokenStream = std::make_unique<antlr4::CommonTokenStream>(pLexer.get());
    pParser = std::make_unique<prologParser>(pTokenStream.get());
}

void ParsingManager::parse() const {
    getStartingRuleNode();
}

}; // namespace Prolog
