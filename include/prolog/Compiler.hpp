#pragma once

#include "prologParser.h"
#include <filesystem>

namespace Prolog {

/**
 * @class Compiler
 * @brief Main class of the prolog compiler
 *
 */
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
     * @param pathToTheFile: path to the target file to compile.
     * @param flags
     */
    void compile(const std::filesystem::path& pathToTheFile, const std::set<Flag>& flags);

private:
    std::filesystem::path m_targetPath;

    void genProlog(prologParser& parser);
    bool enabled(Flag) const;
    void checkSemantics() const;
};
} // namespace Prolog
