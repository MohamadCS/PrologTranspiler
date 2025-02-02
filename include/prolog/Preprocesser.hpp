#pragma once

#include "FrontEndVisitors.hpp"
#include "prologParser.h"
namespace Prolog {

class Preprocesser {
public:
    std::string preprocess(prologParser& parser);
private:
};

} // namespace Prolog
