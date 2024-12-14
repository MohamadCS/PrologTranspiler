#include "Compiler.hpp"
#include "ParsingManager.hpp"
#include "SemanticChecker.hpp"
#include "SyntaxChecker.hpp"
#include "Visitors.hpp"
#include "prologParser.h"
#include <filesystem>
#include <format>
#include <fstream>
#include <util.h>
#include <utility>

namespace Prolog {

void Compiler::genProlog(prologParser& parser) {
    parser.reset();
    auto* programStartCtx = parser.p_text();
    Visitors::ProgramRestoreVisitor progRestoreV;
    Visitors::MarkEmptyTuplesVisitor markEmptyTuplesV;

    markEmptyTuplesV.visit(programStartCtx);

    progRestoreV.emptyTuples = markEmptyTuplesV.emptyTuples;
    progRestoreV.visit(programStartCtx);
    auto progList = progRestoreV.programStmtList;

    auto outputPath = m_targetPath;
    outputPath.replace_extension("").concat("_out.pl");

    std::ofstream outputFile(outputPath);

    if (!outputFile) {
        std::cerr << std::format("ERROR: can't open the file {}\n", outputPath.string());
    }

    for (auto& stmtList : progList) {
        for (auto& stmt : stmtList) {
            outputFile << stmt << " ";
        }
        outputFile << '\n';
    }
}

void Compiler::checkSemantics() const {
    SemanticChecker semanticChecker(m_targetPath);
    semanticChecker.checkUniqueBinding();
    semanticChecker.checkFuncInitVariables();
    semanticChecker.checkInvocImpliesDefine();
    semanticChecker.checkVanishingImpliesBinding();
}

void Compiler::compile(const std::filesystem::path& path, const std::set<Flag>& flags) {
    m_targetPath = path;
    auto parsingManager = ParsingManager(path);

    SyntaxChecker(path).checkSyntax();

    checkSemantics();
}

/**************************************************************************/


} // namespace Prolog
