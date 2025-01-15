#pragma once

#include <filesystem>
#include "prologLexer.h"
#include "prologParser.h"

namespace Prolog {
struct ParsingManager {
    ParsingManager(const std::filesystem::path& path);
    antlr4::tree::ParseTree* getStartingRuleNode() const;
    void parse() const;

    std::filesystem::path targetPath;
    std::unique_ptr<antlr4::ANTLRInputStream> pInputStream;
    std::unique_ptr<prologLexer> pLexer;
    std::unique_ptr<antlr4::CommonTokenStream> pTokenStream;
    std::unique_ptr<prologParser> pParser;
};
} // namespace Prolog
