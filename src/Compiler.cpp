#include "Compiler.hpp"
#include "CodeGen.hpp"
#include "ParsingManager.hpp"
#include "SemanticChecker.hpp"
#include "SyntaxChecker.hpp"
#include "Preprocesser.hpp"
#include "prologParser.h"

#include <filesystem>
#include <format>
#include <fstream>
#include <optional>
#include <sstream>
#include <util.h>

namespace Prolog {


void Compiler::genProlog(prologParser& parser, bool formatOutput) {
    parser.reset();


    // Preprocess
    Preprocesser preprocessor;
    ParsingManager afterPreprocessingParser(preprocessor.preprocess(parser));

    // Get functions
    Visitors::FunctionSemanticsVisitor funcV;
    auto* programStartCtx = afterPreprocessingParser.pParser->p_text();

    funcV.visit(programStartCtx);

    afterPreprocessingParser.pParser->reset();

    // Start CodeGen
    CodeGen::CodeGenVisitor codeGenV(formatOutput);

    codeGenV.setFuncNames(funcV.functionNames);

    std::filesystem::path outputPath;

    programStartCtx = afterPreprocessingParser.pParser->p_text();

    codeGenV.visit(programStartCtx);

    auto newProgList = codeGenV.getCodeBuffer();

    if (m_outputPath.has_value()) {
        outputPath = m_outputPath.value();
    } else {
        outputPath = m_targetPath;
        outputPath.replace_extension("").concat(".pl");
    }

    std::ofstream outputFile(outputPath);

    if (!outputFile) {
        std::cerr << std::format("ERROR: can't open the file {}\n", outputPath.string());
    }

    for (auto& stmtList : newProgList) {
        for (auto& stmt : stmtList) {
            outputFile << stmt;
        }
        outputFile << '\n';
    }
}

inline void Compiler::checkSemantics() const {
    SemanticChecker semanticChecker(m_targetPath);
    semanticChecker.checkInvocImpliesDefine();
    semanticChecker.checkUniqueFuncDef();
    // semanticChecker.checkUniqueBinding();
    // semanticChecker.checkFuncInitVariables();
    // semanticChecker.checkVanishingImpliesBinding();
}

void Compiler::compile(const std::filesystem::path& path, const std::optional<std::filesystem::path>& outputPath,
                       bool disableSemantics, bool formatOutput) {
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

    genProlog(*parsingManager.pParser, formatOutput);
}

/**************************************************************************/

} // namespace Prolog
