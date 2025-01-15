#pragma once

#include <string>
#include <vector>
#include <prologParser.h>


namespace Prolog{
struct Tuple {
    std::vector<std::string> vanishingStmts;
    std::vector<std::string> nonVanishingStmts;
};

struct Predicate {
    std::string name;
    std::vector<std::string> args;
    std::vector<std::string> stmts;
    std::string returnVar;
};

struct TupleEntry : public prologParser::Tuple_entryContext {
    std::string var;
};

}
