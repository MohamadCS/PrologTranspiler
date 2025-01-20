#include <cstring>
#include <iostream>
#include "Compiler.hpp"
#include "gtest.h"
#include "Utils.hpp"
#include "CLITool.hpp"

using namespace antlr4;

int main(int argc, const char** argv) {
    Prolog::CLITool cli;

    return cli.run(argc,argv);

}
