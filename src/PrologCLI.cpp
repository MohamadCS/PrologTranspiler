#include "PrologCLI.hpp"
#include "CLI.hpp"
#include "CLI/CLI.hpp"
#include "Compiler.hpp"

#include "gtest.h"
#include <optional>
namespace Prolog {

CLITool::CLITool()
    : m_app("Compiler for Prolog with functions") {
}

int CLITool::run(int argc, const char** argv) {

    std::filesystem::path inputPath;
    std::optional<std::filesystem::path> outputPath;
    auto* pInputFlagOpt =
        m_app.add_option(CLITool::INPUT_FLAG, inputPath, "Input file path to compile")->check(CLI::ExistingFile);

    m_app.add_option(CLITool::OUTPUT_FLAG, outputPath, "Output file path for compiled prolog")->needs(pInputFlagOpt);

    bool runTests = false, warningsAsErrors = false;

    m_app.add_flag(CLITool::RUN_TESTS_FLAG, runTests, "Run tests");
    m_app.add_flag(CLITool::WARNING_AS_ERRORS_FLAG, warningsAsErrors, "Treat warnings as errors");

    CLI11_PARSE(m_app, argc, argv);

    if (pInputFlagOpt->count()) {
        Prolog::Compiler compiler;
        compiler.compile(inputPath,outputPath);
    }

    if (runTests) {
        testing::InitGoogleTest();
        return RUN_ALL_TESTS();
    }

    if (warningsAsErrors) {
    }

    return 0;
}

}; // namespace Prolog
