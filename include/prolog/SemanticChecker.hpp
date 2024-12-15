#pragma once

#include "ParsingManager.hpp"
#include "Visitors.hpp"
#include <filesystem>

namespace Prolog {


class SemanticChecker {
public:

    enum class Status {
        ERROR = 0,
        FAIL,
        SUCCESS
    };

    SemanticChecker(const std::filesystem::path& path);

    /**
     * @brief Checks if each variable was binded at most once.
     * @return Status::FAIL if some variable was binded more than one time,
     * otherwise Status::SUCCESS.
     */
    Status checkUniqueBinding() const;
    /**
     * @brief Checks if each variable in a function definition was initialized(A
     * function arg or binded).
     * @return Status::FAIL if some variable was not initialized,
     * otherwise Status::SUCCESS.
     */
    Status checkFuncInitVariables() const;
    /**
     * @brief Checks if each function invocation has a function definition somewhere.
     * @return
     */
    Status checkInvocImpliesDefine() const;
    /**
     * @brief Checks if each vanishing statement in a Tuple(Ending with ';') is a binding
     *
     * @return
     */
    Status checkVanishingImpliesBinding() const;

    /**
     * @brief Checks if each function is defined once.
     * @return 
     */
    Status checkUniqueFuncDef() const;

    [[nodiscard]] const Visitors::FunctionSemanticsVisitor& getFuncVisitor() const;

protected:
    const std::filesystem::path m_targetPath;
    Prolog::ParsingManager m_parser;
    Visitors::FunctionSemanticsVisitor m_funcV;

};

} // namespace Prolog
