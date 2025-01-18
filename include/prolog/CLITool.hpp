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

    inline static constexpr std::string INPUT_FLAG =  "-i,--input";
    inline static constexpr std::string OUTPUT_FLAG =  "-o, --output";
    inline static constexpr std::string WARNING_AS_ERRORS_FLAG =  "--Wall";
    inline static constexpr std::string RUN_TESTS_FLAG =  "--run-tests";
    inline static constexpr std::string DISABLE_SEMANTICS =  "--no-semantics";
    inline static constexpr std::string DISABLE_STD_ERR =  "--no-stderr";
};
}; // namespace Prolog
