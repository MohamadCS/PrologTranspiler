#include "Compiler.hpp"
#include "Visitors.hpp"
#include "prologLexer.h"
#include "prologParser.h"
#include <algorithm>
#include <filesystem>
#include <format>
#include <fstream>
#include <memory>
#include <sstream>
#include <util.h>
#include <variant>

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

    auto outputPath = m_targetPath.replace_extension("").concat("_out.pl");

    std::ofstream outputFile(outputPath);

    if (!outputFile) {
        std::cerr << std::format("Error opening the file: {}\n", outputPath.string());
    }

    for (auto& stmtList : progList) {
        for (auto& stmt : stmtList) {
            outputFile << stmt << " ";
        }
        outputFile << '\n';
    }
}

void Compiler::genAst(prologParser& parser) {

    parser.reset();
    auto* programStartCtx = parser.p_text();

    auto outputPath = m_targetPath.replace_extension("").concat("_ast.out");

    std::ofstream outputFile(outputPath);

    if (!outputFile) {
        std::cerr << std::format("Error opening the file: {}\n", outputPath.string());
    }

    outputFile << programStartCtx->toStringTree();
}

void Compiler::varNumCheck(prologParser& parser) {
    parser.reset();
    auto* programStartCtx = parser.p_text();

    Visitors::VariableSemanticVisitor varV;

    varV.visit(programStartCtx);

    for (auto& [varName, count] : varV.varTbl) {
        if (count != Visitors::VariableSemanticVisitor::VAR_COUNT) {
            std::cerr << std::format("Error: {} must appear two times exacly\n", varName);
        }
    }
}

void Compiler::compile(const std::filesystem::path& path, const std::set<Flag>& flags) {
    m_targetPath = path;


    std::ifstream targetFile{path};

    if (!targetFile) {
        std::cerr << std::format("Error opening the file: {}\n", path.string());
    }

    antlr4::ANTLRInputStream input(targetFile);
    prologLexer lexer(&input);
    antlr4::CommonTokenStream tokens(&lexer);
    prologParser parser(&tokens);

    // PERF: Maybe we can change the implementation to some map: Flag -> Func.
    varNumCheck(parser);
    genAst(parser);
    genProlog(parser);
}

} // namespace Prolog
