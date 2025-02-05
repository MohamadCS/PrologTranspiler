#pragma once

#include "FrontEndVisitors.hpp"
#include "prologParser.h"
namespace Prolog {

class Preprocesser {
public:
    /**
     * @brief does the following:
     * Replaces argument'a aliases with their actual variables.
     * Replaces test functions with real functions.
     * If there are test functions, it creates the RunTest/0 function.
     * If there is a main function, it creates a directive at the end of the file, that calls the main predicate.
     * @param parser the parser that contains the tokens to preprocess
     */
    std::string preprocess(prologParser& parser);
private:
};

} // namespace Prolog
