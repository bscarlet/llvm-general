#define __STDC_LIMIT_MACROS

#include <iostream>
#include "LLVM/General/Internal/FFI/Metadata.hpp"
#include "llvm/Support/FormattedStream.h"

#include "llvm/Config/llvm-config.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Metadata.h"
#include "llvm-c/Core.h"

using namespace llvm;

extern "C" {

LLVMMetadataRef LLVM_General_IsAMDString(LLVMMetadataRef md) {
    if (isa<MDString>(unwrap(md))) {
        return md;
    }
    return nullptr;
}

LLVMMetadataRef LLVM_General_MDStringInContext(LLVMContextRef C,
                                               const char *Str, unsigned SLen) {
  return wrap(MDString::get(*unwrap(C), StringRef(Str, SLen)));
}

const char *LLVM_General_GetMDString(LLVMMetadataRef MD, unsigned* Len) {
    if (const MDString *S = dyn_cast<MDString>(unwrap(MD))) {
      *Len = S->getString().size();
      return S->getString().data();
    }
  *Len = 0;
  return nullptr;
}

LLVMMetadataRef LLVM_General_MDValue(LLVMValueRef v) {
    return wrap(ValueAsMetadata::get(unwrap(v)));
}

LLVMMetadataRef LLVM_General_IsAMDNode(LLVMMetadataRef md) {
    if (isa<MDNode>(unwrap(md))) {
        return md;
    }
    return nullptr;
}

LLVMValueRef LLVM_General_GetMDValue(LLVMMetadataRef md) {
    return wrap(unwrap<ValueAsMetadata>(md)->getValue());
}

LLVMMetadataRef LLVM_General_MDNodeInContext(LLVMContextRef C,
                                             LLVMMetadataRef *MDs,
                                             unsigned Count) {
  return wrap(
      MDNode::get(*unwrap(C), ArrayRef<Metadata *>(unwrap(MDs), Count)));
}

LLVMMetadataRef LLVM_General_IsAMDValue(LLVMMetadataRef md) {
    if (isa<ValueAsMetadata>(unwrap(md))) {
        return md;
    }
    return nullptr;
}

unsigned LLVM_General_GetMDKindNames(
	LLVMContextRef c,
	const char **s,
	unsigned *l,
	unsigned n
) {
	SmallVector<StringRef, 8> ns;
	unwrap(c)->getMDKindNames(ns);
	if (ns.size() <= n) {
		for(unsigned i=0; i < ns.size(); ++i) {
			s[i] = ns[i].data();
			l[i] = ns[i].size();
		}
	}
	return ns.size();
}

unsigned LLVM_General_GetMDNodeNumOperands(LLVMMetadataRef v) {
	return unwrap<MDNode>(v)->getNumOperands();
}

void LLVM_General_NamedMetadataAddOperands(
	NamedMDNode *n,
	LLVMMetadataRef *ops,
	unsigned nOps
) {
	for(unsigned i = 0; i != nOps; ++i) n->addOperand(unwrap<MDNode>(ops[i]));
}

const char *LLVM_General_GetNamedMetadataName(
	NamedMDNode *n,
	unsigned *len
) {
	StringRef s = n->getName();
	*len = s.size();
	return s.data();
}

unsigned LLVM_General_GetNamedMetadataNumOperands(NamedMDNode *n) {
	return n->getNumOperands();
}

void LLVM_General_GetNamedMetadataOperands(NamedMDNode *n, LLVMMetadataRef *dest) {
	for(unsigned i = 0; i != n->getNumOperands(); ++i)
		dest[i] = wrap(n->getOperand(i));
}

LLVMMetadataRef LLVM_General_CreateTemporaryMDNodeInContext(LLVMContextRef c) {
	return wrap(MDNode::getTemporary(*unwrap(c), ArrayRef<Metadata *>()).release());
}

void LLVM_General_DestroyTemporaryMDNode(LLVMMetadataRef v) {
    std::cerr << "C: destroy temporary: " << v << "\n";
    MDNode* n = unwrap<MDNode>(v);
    std::cerr << "unwrapped\n";
    std::cerr << "temp? " << n->isTemporary() << "\n";
    n->print(llvm::errs());
	MDNode::deleteTemporary(unwrap<MDNode>(v));
    std::cerr << "C: destroyed temporary\n";
}

void LLVM_General_GetMDNodeOperands(LLVMMetadataRef MD, LLVMMetadataRef *Dest) {
    const auto *N = cast<MDNode>(unwrap(MD));
    const unsigned numOperands = N->getNumOperands();
    for (unsigned i = 0; i < numOperands; i++)
        Dest[i] = wrap(N->getOperand(i));
}

void LLVM_General_MetadataReplaceAllUsesWith(LLVMMetadataRef MD, LLVMMetadataRef New) {
  auto *Node = unwrap<MDNode>(MD);
  Node->replaceAllUsesWith(unwrap<Metadata>(New));
  MDNode::deleteTemporary(Node);
}

}

