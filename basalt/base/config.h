// Copyright (c) 2025 Felix Kahle.
//
// Permission is hereby granted, free of charge, to any person obtaining
// a copy of this software and associated documentation files (the
// "Software"), to deal in the Software without restriction, including
// without limitation the rights to use, copy, modify, merge, publish,
// distribute, sublicense, and/or sell copies of the Software, and to
// permit persons to whom the Software is furnished to do so, subject to
// the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#ifndef BASALT_BASE_CONFIG_H_
#define BASALT_BASE_CONFIG_H_

#ifndef BASALT_CPLUSPLUS_LANG
#if defined(_MSVC_LANG)
#define BASALT_CPLUSPLUS_LANG _MSVC_LANG
#elif defined(__cplusplus)
#define BASALT_CPLUSPLUS_LANG __cplusplus
#endif
#endif // BASALT_CPLUSPLUS_LANG

#ifdef __cpp_constexpr
#define BASALT_COMPILER_SUPPORTS_CONSTEXPR 1
#else
#define BASALT_COMPILER_SUPPORTS_CONSTEXPR 0
#endif // __cpp_constexpr

#ifdef __cpp_concepts
#define BASALT_COMPILER_SUPPORTS_CONCEPTS 1
#else
#define BASALT_COMPILER_SUPPORTS_CONCEPTS 0
#endif // __cpp_concepts

#ifdef __cpp_guaranteed_copy_elision
#define BASALT_COMPILER_SUPPORTS_GUARANTEED_COPY_ELISION 1
#else
#define BASALT_COMPILER_SUPPORTS_GUARANTEED_COPY_ELISION 0
#endif // __cpp_guaranteed_copy_elision

#ifndef BASALT_ALLOW_FORCE_INLINE
#define BASALT_ALLOW_FORCE_INLINE 1
#endif // BASALT_ALLOW_FORCE_INLINE

#if BASALT_ALLOW_FORCE_INLINE
#if defined(_MSC_VER) || defined(__INTEL_COMPILER) || defined(__INTEL_LLVM_COMPILER)
#define BASALT_FORCE_INLINE __forceinline
#elif defined(__GNUC__) || defined(__clang__) || defined(__IBMCPP__) || defined(__SUNPRO_CC)
#define BASALT_FORCE_INLINE inline __attribute__((always_inline))
#else
#define BASALT_FORCE_INLINE inline
#endif
#else
#define BASALT_FORCE_INLINE inline
#endif

#endif // BASALT_BASE_CONFIG_H_
