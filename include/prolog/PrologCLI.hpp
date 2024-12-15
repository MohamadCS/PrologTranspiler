#pragma once

#include <optional>
#include <filesystem>
#include <string_view>
#include <CLI.hpp>

namespace Prolog {
class CLITool{
public:
    CLITool();

    int run(int argc, const char** argv); 
private:
    CLI::App m_app;
    std::optional<std::filesystem::path> m_inputPath;
    std::optional<std::filesystem::path> m_outputPath;
    bool m_runTests;

    inline static constexpr std::string INPUT_FLAG =  "-i";
    inline static constexpr std::string OUTPUT_FLAG =  "-o";
    inline static constexpr std::string WARNING_AS_ERRORS_FLAG =  "--Wall";
    inline static constexpr std::string RUN_TESTS_FLAG =  "--run-tests";
};
}; // namespace Prolog
