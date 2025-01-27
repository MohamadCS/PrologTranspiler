#include "Utils.hpp"
#include <vector>

namespace Prolog::Utility {

std::deque<bool> isVanishingEntryList(prologParser::TupleContext* ctx) {

    const auto& tupleEntriesVec = ctx->tuple_entry();


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
        LOG("Last does not specify operator");
        isVanishing.push_back(false);
    }

    assert(isVanishing.size() == tupleEntriesVec.size());

    return isVanishing;
}

} // namespace Prolog::Utility
