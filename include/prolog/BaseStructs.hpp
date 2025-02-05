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

struct TupleEntry {
    std::string var;
};

struct TestFunc {
    std::string desc;
    std::size_t num;
};

struct Node {
    std::string var;
    bool isEmptyTuple = false;
    bool isPredicate = false;
    std::string predicateText;
};


}
