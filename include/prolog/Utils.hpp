#pragma once

#define ENABLE_DEBUG 0

#if defined(ENABLE_DEBUG) && ENABLE_DEBUG == 1
#define DEBUG(line) line
#define LOG(line) std::cout << "(" << __FUNCTION__ << "," << __LINE__ << "):" << line << std::endl
#else
#define DEBUG(line)
#define LOG(line)
#endif

#define CHECK_NULL(ptr)                                                                                                \
    if (ptr == nullptr) {                                                                                              \
        LOG("Nullptr");                                                                                                \
        throw std::invalid_argument("Null pointer exception");                                                         \
    }

#include "prologParser.h"
#include <functional>

namespace Prolog::Utility {
std::deque<bool> isVanishingEntryList(prologParser::TupleContext* ctx);

template <class Container>
void print(Container& container) {
    std::cout << "[";
    std::for_each(container.begin(), container.end(), [](auto&& elm) { std::cout << elm << ","; });

    std::cout << "]\n";
}

template <class Iterator>
std::string convertContainerToListStr(Iterator begin, Iterator end, std::function<std::string(Iterator)> itemFormat,
                                      const std::variant<std::string, std::vector<std::string>>& sep = ",") {
    std::stringstream columnsQuery;

    std::function<std::string(int)> genSep;

    if (auto pSep = std::get_if<std::vector<std::string>>(&sep)) {
        genSep = [&pSep](int i) { return (*pSep)[i]; }; // PERF: Find a way to replace the function call.
    } else {
        genSep = [&sep](int i) { return std::get<std::string>(sep); };
    }

    int idx = 0;
    for (Iterator it = begin; it != end; ++it) {
        columnsQuery << itemFormat(it);

        if (std::next(it) != end) {
            columnsQuery << genSep(idx++);
        }
    }

    return columnsQuery.str();
}
} // namespace Prolog::Utility
