#define __STDC_LIMIT_MACROS
#include "llvm-c/Core.h"
#include "llvm/IR/Value.h"
#include "LLVM/General/Internal/FFI/Value.h"

using namespace llvm;

extern "C" {

LLVMValueSubclassId LLVM_General_GetValueSubclassId(LLVMValueRef v) {
	switch(unwrap(v)->getValueID()) {
#define VALUE_SUBCLASS_ID_CASE(class) case Value::class ## Val: return LLVM ## class ## SubclassId;
LLVM_GENERAL_FOR_EACH_VALUE_SUBCLASS(VALUE_SUBCLASS_ID_CASE)
#undef VALUE_SUBCLASS_ID_CASE
	default: break;
	}
	return LLVMValueSubclassId(0);
}

}
