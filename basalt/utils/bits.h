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

#ifndef BASALT_UTILS_BIT_H_
#define BASALT_UTILS_BIT_H_

#include "basalt/base/config.h"
#if defined(_MSC_VER)
#include <intrin.h>
#endif
#include "absl/log/check.h"

namespace bslt::bits
{
    BASALT_FORCE_INLINE uint64_t LeastSignificantBitPosition64(const uint64_t value) noexcept
    {
        DCHECK_NE(value, 0ULL) << "LSB position is undefined for value 0";

#if defined(__GNUC__) || defined(__llvm__)
        return __builtin_ctzll(value);
#elif defined(_MSC_VER) && (defined(_M_X64) || defined(_M_ARM64))
        unsigned long index = 0;
        _BitScanForward64(&index, value);
        return index;

#else
        // Fallback for: Non-Compiler-Specific OR MSVC 32-bit
        static const uint64_t kSeq = 0x0218a392dd5fb34fULL;
        static const uint8_t kTab[64] = {
            0, 1, 2, 7, 3, 13, 8, 19, 4, 25, 14, 28, 9, 52, 20, 58,
            5, 17, 26, 56, 15, 38, 29, 40, 10, 49, 53, 31, 21, 34, 59, 42,
            63, 6, 12, 18, 24, 27, 51, 57, 16, 55, 37, 39, 48, 30, 33, 41,
            62, 11, 23, 50, 54, 36, 47, 32, 61, 22, 35, 46, 60, 45, 44, 43,
        };
        return kTab[((value & (~value + 1)) * kSeq) >> 58];
#endif
    }

    BASALT_FORCE_INLINE uint32_t LeastSignificantBitPosition32(const uint32_t value) noexcept
    {
        DCHECK_NE(value, 0U) << "LSB position is undefined for value 0";

#if defined(__GNUC__) || defined(__llvm__)
        return __builtin_ctz(value);

        // _BitScanForward works on both 32-bit and 64-bit MSVC
#elif defined(_MSC_VER)
        unsigned long index = 0;
        _BitScanForward(&index, value);
        return index;

#else
        return (uint32_t)LeastSignificantBitPosition64(static_cast<uint64_t>(value));
#endif
    }

    BASALT_FORCE_INLINE uint16_t LeastSignificantBitPosition16(const uint16_t value) noexcept
    {
        DCHECK_NE(value, 0U) << "LSB position is undefined for value 0";

        return static_cast<uint16_t>(LeastSignificantBitPosition32(value));
    }

    BASALT_FORCE_INLINE uint8_t LeastSignificantBitPosition8(const uint8_t value) noexcept
    {
        DCHECK_NE(value, 0U) << "LSB position is undefined for value 0";

        return static_cast<uint8_t>(LeastSignificantBitPosition32(value));
    }

    // -----------------------------------------------------------------------------
    // Bit Position (Index within the word)
    // -----------------------------------------------------------------------------
    BASALT_FORCE_INLINE uint32_t BitPos64(const uint64_t pos) noexcept
    {
        return (pos & 63);
    }

    BASALT_FORCE_INLINE uint32_t BitPos32(const uint32_t pos) noexcept
    {
        return (pos & 31);
    }

    BASALT_FORCE_INLINE uint32_t BitPos16(const uint16_t pos) noexcept
    {
        return (pos & 15);
    }

    BASALT_FORCE_INLINE uint32_t BitPos8(const uint8_t pos) noexcept
    {
        return (pos & 7);
    }

    // -----------------------------------------------------------------------------
    // Bit Offset (Index of the word in an array)
    // -----------------------------------------------------------------------------
    BASALT_FORCE_INLINE uint64_t BitOffset64(const uint64_t pos) noexcept
    {
        return (pos >> 6);
    }

    BASALT_FORCE_INLINE uint32_t BitOffset32(const uint32_t pos) noexcept
    {
        return (pos >> 5);
    }

    BASALT_FORCE_INLINE uint16_t BitOffset16(const uint16_t pos) noexcept
    {
        return (pos >> 4);
    }

    BASALT_FORCE_INLINE uint8_t BitOffset8(const uint8_t pos) noexcept
    {
        return (pos >> 3);
    }

    // -----------------------------------------------------------------------------
    // Bit Length (Allocation size required for N bits)
    // Note: 'size' input changed to uint64_t to allow large bit counts
    // -----------------------------------------------------------------------------
    BASALT_FORCE_INLINE uint64_t BitLength64(const uint64_t size) noexcept
    {
        return ((size + 63) >> 6);
    }

    BASALT_FORCE_INLINE uint32_t BitLength32(const uint64_t size) noexcept
    {
        return static_cast<uint32_t>((size + 31) >> 5);
    }

    BASALT_FORCE_INLINE uint16_t BitLength16(const uint64_t size) noexcept
    {
        return static_cast<uint16_t>((size + 15) >> 4);
    }

    BASALT_FORCE_INLINE uint8_t BitLength8(const uint64_t size) noexcept
    {
        return static_cast<uint8_t>((size + 7) >> 3);
    }
}

#endif // BASALT_UTILS_BIT_H_
