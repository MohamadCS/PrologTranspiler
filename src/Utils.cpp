#include "Utils.hpp"
#include <vector>

namespace Prolog::Utility {
Prolog::Tuple createTuple(prologParser::TupleContext* ctx) {

    const auto& tupleEntriesVec = ctx->tuple_entry();

    std::vector<std::string> vanishingStmts;
    std::vector<std::string> nonVanishingStmts;

    std::deque<bool> isVanishing;

    for (auto& child : ctx->children) {
        if (child->getText() == ";") {
            isVanishing.push_back(true);
        }
        if (child->getText() == ",") {
            isVanishing.push_back(false);
        }
    }

    // Last element does not specify ',' or ';' which means its non-vanishing
    if (isVanishing.size() < tupleEntriesVec.size()) {
        isVanishing.push_back(false);
    }

    // PERF: reserve storage before filling the vec.
    for (int i = 0; i < tupleEntriesVec.size(); ++i) {
        auto* pEntry = tupleEntriesVec[i];
        if (isVanishing[i]) {
            vanishingStmts.push_back(pEntry->getText());
        } else {
            nonVanishingStmts.push_back(pEntry->getText());
        }
    }

    return Prolog::Tuple(std::move(vanishingStmts), std::move(nonVanishingStmts));
}

} // namespace Prolog::Utility
