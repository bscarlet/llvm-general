#ifndef __LLVM_GENERAL_INTERNAL_FFI__CONSTANT__H__
#define __LLVM_GENERAL_INTERNAL_FFI__CONSTANT__H__

#define LLVM_GENERAL_FOR_EACH_FLOAT_SEMANTICS(macro) \
	macro(IEEEhalf) \
	macro(IEEEsingle) \
	macro(IEEEdouble) \
	macro(IEEEquad) \
	macro(PPCDoubleDouble) \
	macro(x87DoubleExtended) \
	macro(Bogus)

typedef enum {
#define ENUM_CASE(x) LLVMFloatSemantics ## x,
LLVM_GENERAL_FOR_EACH_FLOAT_SEMANTICS(ENUM_CASE)
#undef ENUM_CASE
} LLVMFloatSemantics;


#endif
