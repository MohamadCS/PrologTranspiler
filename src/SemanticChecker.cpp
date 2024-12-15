#include "SemanticChecker.hpp"

namespace Prolog {
SemanticChecker::SemanticChecker(const std::filesystem::path& targetPath)
    : m_targetPath(targetPath),
      m_parser(targetPath),
      m_funcV() {
    m_parser.pParser->reset();
    auto* pStartCtx = m_parser.getStartingRuleNode();
    m_funcV.visit(pStartCtx);
}

const Visitors::FunctionSemanticsVisitor& SemanticChecker::getFuncVisitor() const {
    return m_funcV;
}

SemanticChecker::Status SemanticChecker::checkFuncInitVariables() const {
    auto& funcsVars = m_funcV.funcsVars;
    auto& initVars = m_funcV.initializedVars;
    auto& funcsNames = m_funcV.functionNames;

    std::size_t funcsNum = initVars.size();

    Status status = Status::SUCCESS;

    for (std::size_t i = 0; i < funcsNum; ++i) {
        std::set<std::string> setDiff;

        std::set_difference(funcsVars[i].begin(), funcsVars[i].end(), initVars[i].begin(), initVars[i].end(),
                            std::inserter(setDiff, setDiff.begin()));

        if (!setDiff.empty()) {

            auto printUninitVars = [i, &funcsNames](auto&& var) {
                std::cerr << std::format("ERROR: Variable {} is not initialized in function {}.\n",
                                         std::forward<decltype(var)>(var), funcsNames[i]);
            };

            status = Status::FAIL;
            std::for_each(setDiff.begin(), setDiff.end(), std::ref(printUninitVars));
        }
    }

    return status;
}

SemanticChecker::Status SemanticChecker::checkUniqueBinding() const {

    auto& bindedVars = m_funcV.bindedVars;
    auto& funcsNames = m_funcV.functionNames;

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

SemanticChecker::Status SemanticChecker::checkInvocImpliesDefine() const {

    auto funcNamesSet = std::set<std::string>(m_funcV.functionNames.begin(), m_funcV.functionNames.end());
    auto& funcsInvockSet = m_funcV.functionInvoc;

    std::set<std::string> setDiff;

    // Compute funcInvokeSet / funcNamesSet, if its an empty set, then all funcInvokes are legal.
    std::set_difference(funcsInvockSet.begin(), funcsInvockSet.end(), funcNamesSet.begin(), funcNamesSet.end(),
                        std::inserter(setDiff, setDiff.end()));

    if (setDiff.empty()) {
        return Status::SUCCESS;
    }

    for (const auto& funcName : setDiff) {
        std::cerr << std::format("ERROR: Function {} is invoked but not defined.\n", funcName);
    }

    return Status::FAIL;
}

SemanticChecker::Status SemanticChecker::checkVanishingImpliesBinding() const {

    if (m_funcV.vanishingNoBinding.empty()) {
        return Status::SUCCESS;
    }

    for (auto& bindingNode : m_funcV.vanishingNoBinding) {
        std::cerr << std::format("WARNING: Vanishing statement {} is not a binding.\n", bindingNode->getText());
    }

    return Status::FAIL;
}

SemanticChecker::Status SemanticChecker::checkUniqueFuncDef() const {
    auto& funcsNameVec = m_funcV.functionNames;

    std::set<std::string> funcsNamesSet;
    Status status = Status::SUCCESS;

    for (auto& funcName : funcsNameVec) {
        if (funcsNamesSet.find(funcName) != funcsNamesSet.end()) {
            status = Status::FAIL;
            std::cerr << std::format("ERROR: Function {} is already defined.\n", funcName);
        } else{
            funcsNamesSet.insert(funcName);
        }
    }

    return status;
}

}; // namespace Prolog
