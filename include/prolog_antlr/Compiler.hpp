#pragma once

#include "prologLexer.h"
#include "prologParser.h"
#include "tree/ParseTree.h"
#include <filesystem>
#include <memory>

namespace Prolog {

/**
 * @class Compiler
 * @brief Main class of the prolog compiler
 *
 */

struct ParsingManager {
    ParsingManager(const std::filesystem::path& path);
    antlr4::tree::ParseTree* getStartingRuleNode() const;

    std::filesystem::path pTargetPath;
    std::unique_ptr<antlr4::ANTLRInputStream> pInputStream;
    std::unique_ptr<prologLexer> pLexer;
    std::unique_ptr<antlr4::CommonTokenStream> pTokenStream;
    std::unique_ptr<prologParser> pParser;
};

class Compiler {
public:
    enum class Flag {
        VAR_NUM_CHECK = 0,
        COMP_TO_PROLOG,
        GEN_AST,
    };

    Compiler() = default;
    ~Compiler() = default;

    /**
     * @brief Compiles the file to prolog, with additional semantics.
     *
     * @param pathToTheFile: path to the traget file to compile.
     * @param flags
     */
    void compile(const std::filesystem::path& pathToTheFile, const std::set<Flag>& flags);

private:
    std::filesystem::path m_targetPath;

    void varNumCheck(prologParser& parser);
    void genAst(prologParser& parser);
    void genProlog(prologParser& parser);

    bool enabled(Flag) const;
};
} // namespace Prolog
