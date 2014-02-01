#define __STDC_LIMIT_MACROS
#include "llvm/Support/raw_ostream.h"
#include "llvm-c/Core.h"

using namespace llvm;
static const unsigned F_None = 0;
static const unsigned F_Excl = raw_fd_ostream::F_Excl;
static const unsigned F_Binary = raw_fd_ostream::F_Binary;

extern "C" {

LLVMBool LLVM_General_WithFileRawOStream(
	const char *filename,
	LLVMBool excl,
	LLVMBool binary,
	const char *&error,
	void (&callback)(raw_ostream &ostream)
) {
	std::string e;
	raw_fd_ostream os(filename, e, (excl ? F_Excl : F_None) | (binary ? F_Binary : F_None));
	if (!e.empty()) {
		error = strdup(e.c_str());
		return false;
	}
	callback(os);
	return true;
}

void LLVM_General_WithBufferRawOStream(
	void (&outputCallback)(const char *start, size_t length),
	void (&streamCallback)(raw_ostream &ostream)
) {
	std::string s;
	{
		raw_string_ostream os(s);
		streamCallback(os);
	}
	outputCallback(s.data(), s.size());
}
	
}
