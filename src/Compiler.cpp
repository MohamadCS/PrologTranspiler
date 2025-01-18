#include "Compiler.hpp"
#include "CodeGen.hpp"
#include "FrontEndVisitors.hpp"
#include "ParsingManager.hpp"
#include "SemanticChecker.hpp"
#include "SyntaxChecker.hpp"
#include "prologParser.h"

#include <filesystem>
#include <format>
#include <fstream>
#include <optional>
#include <util.h>

namespace Prolog {

void Compiler::genProlog(prologParser& parser) {
    parser.reset();
    auto* programStartCtx = parser.p_text();

    CodeGen::CodeGenVisitor codeGenV;

    codeGenV.visit(programStartCtx);
    auto progList = codeGenV.getCodeBuffer();

    std::filesystem::path outputPath;

    if (m_outputPath.has_value()) {
        outputPath = m_outputPath.value();
    } else {
        outputPath = m_targetPath;
        outputPath.replace_extension("").concat("_out.pl");
    }

    std::ofstream outputFile(outputPath);

    if (!outputFile) {
        std::cerr << std::format("ERROR: can't open the file {}\n", outputPath.string());
    }

    for (auto& stmtList : progList) {
        for (auto& stmt : stmtList) {
            outputFile << stmt;
        }
        outputFile << '\n';
    }
}

inline void Compiler::checkSemantics() const {
    SemanticChecker semanticChecker(m_targetPath);
    semanticChecker.checkUniqueBinding();
    semanticChecker.checkFuncInitVariables();
    semanticChecker.checkInvocImpliesDefine();
    semanticChecker.checkVanishingImpliesBinding();
    semanticChecker.checkUniqueFuncDef();
}

void Compiler::compile(const std::filesystem::path& path, const std::optional<std::filesystem::path>& outputPath,
                       bool disableSemantics) {
    m_targetPath = path;
    m_outputPath = outputPath;
    auto parsingManager = ParsingManager(path);

    if (SyntaxChecker(path).checkSyntax() == SyntaxChecker::Status::FAIL) {
        std::cerr << "ERROR: Invalid Syntax\n";
        exit(-1);
    }

    if (!disableSemantics) {
        checkSemantics();
    }

    genProlog(*parsingManager.pParser);
}

/**************************************************************************/

} // namespace Prolog
