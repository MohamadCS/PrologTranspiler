#include "Preprocesser.hpp"
#include "prologParser.h"

namespace Prolog {

std::string Preprocesser::preprocess(prologParser& parser) {

    Visitors::PreprocessorVisitor progV;
    auto* programStartCtx = parser.p_text();

    progV.visit(programStartCtx);
    auto progList = progV.programStmtList;

    std::stringstream procSStr;

    for (auto& stmtList : progList) {
        for (auto& stmt : stmtList) {
            procSStr << stmt << " ";
        }
        procSStr << '\n';
    }

    if(!progV.testFuncList.empty()){
        procSStr << "RunTests() :: ( \n";

        for (const auto& testFunc : progV.testFuncList) {
            procSStr << std::format("write(\"RUNNING TEST {} : {}\\n\");\n",testFunc.num,testFunc.desc);
            procSStr << std::format("Test{}();\n",testFunc.num);
        }
        procSStr << ")\n.\n";
    }

    if(progV.mainDefined){
        procSStr << ":- main.";
    }


    return procSStr.str();
}

} // namespace Prolog
