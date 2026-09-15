/*-------------------------------------------------------------------------
 *
 * llvmjit_wrap.cpp
 *	  Parts of the LLVM interface not (yet) exposed to C.
 *
 * Copyright (c) 2016-2026, PostgreSQL Global Development Group
 *
 * IDENTIFICATION
 *	  src/backend/lib/llvm/llvmjit_wrap.cpp
 *
 *-------------------------------------------------------------------------
 */

extern "C"
{
#include "postgres.h"
}

/*
 * LLVM's headers produce warnings under some of the warning options we
 * use for our own code (at least -Wshadow=local is known to be
 * problematic), so turn those off while including them.  Note that gcc
 * attributes each warning to the most specific option that covers it,
 * so all the -Wshadow variants have to be listed separately; and the
 * "-Wshadow=..." spellings are unknown to clang, which would complain
 * about an unknown warning group, hence the __clang__ check.
 */
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wshadow"
#ifndef __clang__
#pragma GCC diagnostic ignored "-Wshadow=local"
#pragma GCC diagnostic ignored "-Wshadow=compatible-local"
#endif
#endif
#include <llvm-c/Core.h>
#include <llvm/IR/Function.h>
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif

#include "jit/llvmjit.h"
#include "jit/llvmjit_backport.h"

#ifdef USE_LLVM_BACKPORT_SECTION_MEMORY_MANAGER
#ifdef __GNUC__
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wshadow"
#ifndef __clang__
#pragma GCC diagnostic ignored "-Wshadow=local"
#pragma GCC diagnostic ignored "-Wshadow=compatible-local"
#endif
#endif
#include <llvm/ExecutionEngine/Orc/RTDyldObjectLinkingLayer.h>
#include <llvm/ExecutionEngine/SectionMemoryManager.h>
#include "jit/SectionMemoryManager.h"
#include <llvm/Support/CBindingWrapping.h>
#ifdef __GNUC__
#pragma GCC diagnostic pop
#endif
#endif


/*
 * C-API extensions.
 */

LLVMTypeRef
LLVMGetFunctionReturnType(LLVMValueRef r)
{
	return llvm::wrap(llvm::unwrap<llvm::Function>(r)->getReturnType());
}

LLVMTypeRef
LLVMGetFunctionType(LLVMValueRef r)
{
	return llvm::wrap(llvm::unwrap<llvm::Function>(r)->getFunctionType());
}

#ifdef USE_LLVM_BACKPORT_SECTION_MEMORY_MANAGER
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(llvm::orc::ExecutionSession, LLVMOrcExecutionSessionRef)
DEFINE_SIMPLE_CONVERSION_FUNCTIONS(llvm::orc::ObjectLayer, LLVMOrcObjectLayerRef);

LLVMOrcObjectLayerRef
LLVMOrcCreateRTDyldObjectLinkingLayerWithSafeSectionMemoryManager(LLVMOrcExecutionSessionRef ES)
{
#if LLVM_VERSION_MAJOR >= 21
	return wrap(new llvm::orc::RTDyldObjectLinkingLayer(
		*unwrap(ES), [](const llvm::MemoryBuffer&) {
			return std::make_unique<llvm::backport::SectionMemoryManager>(nullptr, true);
		}));
#else
	return wrap(new llvm::orc::RTDyldObjectLinkingLayer(
		*unwrap(ES), [] { return std::make_unique<llvm::backport::SectionMemoryManager>(nullptr, true); }));
#endif
}
#endif
