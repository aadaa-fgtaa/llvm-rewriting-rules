#include <unordered_set>
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Passes/PassPlugin.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Transforms/Scalar/DCE.h"

template<typename T, size_t sz> inline size_t lengthof(T (&)[sz]) { return sz; }

extern "C" void hs_init(int *argc, char **argv[]);
extern "C" void hs_exit();

struct RTS {
  RTS() {
    static char* argv[] = { (char*) "rulesPass.so", 0 }, **argv_ = argv;
    static int argc_ = lengthof(argv) - 1;
    hs_init(&argc_, &argv_);
  }
  ~RTS() {
    hs_exit();
  }
} rts;


using namespace std;
using namespace llvm;

extern "C" bool rulesPlugin(Instruction*);

struct RulesPass : PassInfoMixin<RulesPass> {
  PreservedAnalyses run(Function &fn, FunctionAnalysisManager &fam) {
    bool changed = false;
    bool changedLastTime = true;

    unordered_set<Instruction*> rewriten;

    while (changedLastTime) {
      changedLastTime = false;

      for (auto& block: fn) {
        for(auto& instr: block) {
          if (!rewriten.count(&instr) && rulesPlugin(&instr)) {
            changedLastTime = true;
            rewriten.insert(&instr);
          }
        }
      }

      changed |= changedLastTime;
    }

    return changed ? PreservedAnalyses::none() : PreservedAnalyses::all();
  }
};


extern "C" LLVM_ATTRIBUTE_WEAK PassPluginLibraryInfo llvmGetPassPluginInfo() {
  return {
    LLVM_PLUGIN_API_VERSION,
    "rules",
    LLVM_VERSION_STRING,
    [](PassBuilder &PB) {
      PB.registerPipelineParsingCallback(
        [](StringRef Name, FunctionPassManager &FPM, ArrayRef<PassBuilder::PipelineElement>) {
          if (Name == "rules") {
            FPM.addPass(RulesPass());
            FPM.addPass(VerifierPass());
            FPM.addPass(DCEPass());
            return true;
          }
          return false;
        }
      );
    }
  };
}
