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

// ReSharper disable CppDFAUnreachableFunctionCall

#ifndef BASALT_UTILS_BIT_H_
#define BASALT_UTILS_BIT_H_

#include "basalt/base/config.h"
#if defined(_MSC_VER)
#include <intrin.h>
#endif // defined(_MSC_VER)
#include "absl/log/check.h"

#include <type_traits>
#include <cstdint>

namespace bslt::bits
{
    // Useful bitmask constants

    constexpr uint64_t kAllBits64 = uint64_t{0xFFFFFFFFFFFFFFFF};
    constexpr uint64_t kAllBitsButLsb64 = uint64_t{0xFFFFFFFFFFFFFFFE};
    constexpr uint32_t kAllBits32 = 0xFFFFFFFFU;
    constexpr uint32_t kAllBitsButLsb32 = 0xFFFFFFFEU;
    constexpr uint16_t kAllBits16 = 0xFFFFU;
    constexpr uint16_t kAllBitsButLsb16 = 0xFFFEU;
    constexpr uint8_t kAllBits8 = 0xFFU;
    constexpr uint8_t kAllBitsButLsb8 = 0xFEU;

    /// @brief Creates a 64-bit value with a single bit set at the specified position.
    ///
    /// @param pos The 0-based index of the bit to set (0-63).
    /// @return A uint64_t with only the bit at 'pos' set to 1.
    /// @note The behavior is undefined if pos >= 64.
    constexpr BASALT_FORCE_INLINE uint64_t OneBit64(const uint32_t pos) noexcept
    {
        DCHECK_LT(pos, 64U) << "Shift amount must be less than 64";

        return uint64_t{1} << pos;
    }

    /// @brief Creates a 32-bit value with a single bit set at the specified position.
    ///
    /// @param pos The 0-based index of the bit to set (0-31).
    /// @return A uint32_t with only the bit at 'pos' set to 1.
    /// @note The behavior is undefined if pos >= 32.
    constexpr BASALT_FORCE_INLINE uint32_t OneBit32(const uint32_t pos) noexcept
    {
        DCHECK_LE(pos, 31U) << "Shift amount must be less than 32";

        return 1U << pos;
    }

    /// @brief Creates a 16-bit value with a single bit set at the specified position.
    ///
    /// @param pos The 0-based index of the bit to set (0-15).
    /// @return A uint16_t with only the bit at 'pos' set to 1.
    /// @note The behavior is undefined if pos >= 16.
    constexpr BASALT_FORCE_INLINE uint16_t OneBit16(const uint32_t pos) noexcept
    {
        DCHECK_LE(pos, 15U) << "Shift amount must be less than 16";

        return static_cast<uint16_t>(1U << pos);
    }

    /// @brief Creates an 8-bit value with a single bit set at the specified position.
    ///
    /// @param pos The 0-based index of the bit to set (0-7).
    /// @return A uint8_t with only the bit at 'pos' set to 1.
    /// @note The behavior is undefined if pos >= 8.
    constexpr BASALT_FORCE_INLINE uint8_t OneBit8(const uint32_t pos) noexcept
    {
        DCHECK_LE(pos, 7U) << "Shift amount must be less than 8";

        return static_cast<uint8_t>(1U << pos);
    }

    /// @brief Generic template to create a value with a single bit set.
    ///
    /// Dispatches to the appropriate size-specific implementation based on type T.
    ///
    /// @tparam T An unsigned integer type.
    /// @param pos The 0-based index of the bit to set.
    /// @return A value of type T with only the bit at 'pos' set to 1.
    // ReSharper disable once CppNotAllPathsReturnValue
    template <typename T>
        requires std::is_unsigned_v<T>
    constexpr BASALT_FORCE_INLINE T OneBit(const uint32_t pos) noexcept
    {
        if constexpr (std::is_same_v<T, uint64_t>)
        {
            return OneBit64(pos);
        }
        else if constexpr (std::is_same_v<T, uint32_t>)
        {
            return OneBit32(pos);
        }
        else if constexpr (std::is_same_v<T, uint16_t>)
        {
            return OneBit16(pos);
        }
        else if constexpr (std::is_same_v<T, uint8_t>)
        {
            return OneBit8(pos);
        }
        else
        {
            static_assert(sizeof(T) == 0, "Unsupported type for OneBit");
        }
    }

    /// @brief Calculates the population count (number of set bits) in a 64-bit integer.
    ///
    /// Uses a SWAR (SIMD Within A Register) algorithm for constant-time execution.
    ///
    /// @param n The value to count bits in.
    /// @return The number of bits set to 1.
    constexpr BASALT_FORCE_INLINE uint64_t BitCount64(uint64_t n) noexcept
    {
        constexpr auto m1 = uint64_t{0x5555555555555555};
        constexpr auto m2 = uint64_t{0x3333333333333333};
        constexpr auto m4 = uint64_t{0x0F0F0F0F0F0F0F0F};
        constexpr auto h01 = uint64_t{0x0101010101010101};
        n -= (n >> 1) & m1;
        n = (n & m2) + ((n >> 2) & m2);
        n = (n + (n >> 4)) & m4;
        n = (n * h01) >> 56;
        return n;
    }

    /// @brief Calculates the population count (number of set bits) in a 32-bit integer.
    ///
    /// @param n The value to count bits in.
    /// @return The number of bits set to 1.
    constexpr BASALT_FORCE_INLINE uint32_t BitCount32(uint32_t n) noexcept
    {
        n -= (n >> 1) & 0x55555555UL;
        n = (n & 0x33333333) + ((n >> 2) & 0x33333333UL);
        n = (n + (n >> 4)) & 0x0F0F0F0FUL;
        n = n + (n >> 8);
        n = n + (n >> 16);
        return n & 0x0000003FUL;
    }

    /// @brief Calculates the population count (number of set bits) in a 16-bit integer.
    ///
    /// @param n The value to count bits in.
    /// @return The number of bits set to 1.
    constexpr BASALT_FORCE_INLINE uint16_t BitCount16(uint16_t n) noexcept
    {
        n -= (n >> 1) & 0x5555U;
        n = (n & 0x3333U) + ((n >> 2) & 0x3333U);
        n = (n + (n >> 4)) & 0x0F0FU;
        return static_cast<uint16_t>((n + (n >> 8)) & 0x001FU);
    }

    /// @brief Calculates the population count (number of set bits) in an 8-bit integer.
    ///
    /// @param n The value to count bits in.
    /// @return The number of bits set to 1.
    constexpr BASALT_FORCE_INLINE uint8_t BitCount8(uint8_t n) noexcept
    {
        n = static_cast<uint8_t>(n - ((n >> 1) & 0x55U));
        n = static_cast<uint8_t>((n & 0x33U) + ((n >> 2) & 0x33U));
        return static_cast<uint8_t>((n + (n >> 4)) & 0x0FU);
    }

    /// @brief Isolates the least significant bit of a 64-bit integer.
    ///
    /// If n is 0, returns 0. Otherwise, returns a value with only the least significant bit of n set.
    ///
    /// @param n The value to process.
    /// @return A value with only the least significant bit of n set.
    constexpr BASALT_FORCE_INLINE uint64_t LeastSignificantBitWord64(const uint64_t n) noexcept
    {
        return n & ~(n - 1);
    }

    /// @brief Isolates the least significant bit of a 32-bit integer.
    ///
    /// If n is 0, returns 0. Otherwise, returns a value with only the least significant bit of n set.
    ///
    /// @param n The value to process.
    /// @return A value with only the least significant bit of n set.
    constexpr BASALT_FORCE_INLINE uint32_t LeastSignificantBitWord32(const uint32_t n) noexcept
    {
        return n & ~(n - 1);
    }

    /// @brief Isolates the least significant bit of a 16-bit integer.
    ///
    /// If n is 0, returns 0. Otherwise, returns a value with only the least significant bit of n set.
    ///
    /// @param n The value to process.
    /// @return A value with only the least significant bit of n set.
    constexpr BASALT_FORCE_INLINE uint16_t LeastSignificantBitWord16(const uint16_t n) noexcept
    {
        return n & static_cast<uint16_t>(~(n - 1));
    }

    /// @brief Isolates the least significant bit of an 8-bit integer.
    ///
    /// If n is 0, returns 0. Otherwise, returns a value with only the least significant bit of n set.
    ///
    /// @param n The value to process.
    /// @return A value with only the least significant bit of n set.
    constexpr BASALT_FORCE_INLINE uint8_t LeastSignificantBitWord8(const uint8_t n) noexcept
    {
        return n & static_cast<uint8_t>(~(n - 1));
    }

    /// @brief Generic template to isolate the least significant bit of any unsigned integer type.
    ///
    /// @param n The value to process.
    /// @return A value with only the least significant bit of n set.
    template <typename T>
        requires std::is_unsigned_v<T>
    constexpr BASALT_FORCE_INLINE T LeastSignificantBitWord(const T n) noexcept
    {
        return n & static_cast<T>(~(n - 1));
    }

    /// @brief Calculates the index of the least significant bit using a De Bruijn sequence.
    ///
    /// This is a compile-time capable fallback for architectures without native CTZ intrinsics.
    ///
    /// @param n The 64-bit integer to check. Must not be 0.
    /// @return The 0-based index of the least significant bit (0-63).
    /// @note The behavior is undefined if n is 0.
    constexpr BASALT_FORCE_INLINE int LeastSignificantBitPosition64DeBruijn(const uint64_t n) noexcept
    {
        DCHECK_NE(n, 0ULL) << "LSB position is undefined for value 0";

        constexpr auto kSeq = uint64_t{0x0218a392dd5fb34f};
        constexpr int kTab[64] = {
            0, 1, 2, 7, 3, 13, 8, 19, 4, 25, 14, 28, 9, 52, 20, 58,
            5, 17, 26, 56, 15, 38, 29, 40, 10, 49, 53, 31, 21, 34, 59, 42,
            63, 6, 12, 18, 24, 27, 51, 57, 16, 55, 37, 39, 48, 30, 33, 41,
            62, 11, 23, 50, 54, 36, 47, 32, 61, 22, 35, 46, 60, 45, 44, 43,
        };

        return kTab[((n & (~n + 1)) * kSeq) >> 58];
    }

    /// @brief Returns the index of the least significant bit set in a 64-bit integer.
    ///
    /// Uses hardware intrinsics (bsf/ctz) where available, falling back to De Bruijn multiplication.
    ///
    /// @param value The value to scan. Must not be 0.
    /// @return The 0-based index of the least significant bit (0-63).
    /// @note The behavior is undefined if value is 0.
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
        return LeastSignificantBitPosition64DeBruijn(value);
#endif // defined(__GNUC__) || defined(__llvm__)
    }

    /// @brief Returns the index of the least significant bit set in a 32-bit integer.
    ///
    /// @param value The value to scan. Must not be 0.
    /// @return The 0-based index of the least significant bit (0-31).
    /// @note The behavior is undefined if value is 0.
    BASALT_FORCE_INLINE uint32_t LeastSignificantBitPosition32(const uint32_t value) noexcept
    {
        DCHECK_NE(value, 0U) << "LSB position is undefined for value 0";

#if defined(__GNUC__) || defined(__llvm__)
        return __builtin_ctz(value);
#elif defined(_MSC_VER)
        unsigned long index = 0;
        _BitScanForward(&index, value);
        return index;
#else
        return static_cast<uint32_t>(LeastSignificantBitPosition64(static_cast<uint64_t>(value)));
#endif // defined(__GNUC__) || defined(__llvm__)
    }

    /// @brief Returns the index of the least significant bit set in a 16-bit integer.
    ///
    /// @param value The value to scan. Must not be 0.
    /// @return The 0-based index of the least significant bit (0-15).
    BASALT_FORCE_INLINE uint16_t LeastSignificantBitPosition16(const uint16_t value) noexcept
    {
        DCHECK_NE(value, 0U) << "LSB position is undefined for value 0";

        return static_cast<uint16_t>(LeastSignificantBitPosition32(value));
    }

    /// @brief Returns the index of the least significant bit set in an 8-bit integer.
    ///
    /// @param value The value to scan. Must not be 0.
    /// @return The 0-based index of the least significant bit (0-7).
    BASALT_FORCE_INLINE uint8_t LeastSignificantBitPosition8(const uint8_t value) noexcept
    {
        DCHECK_NE(value, 0U) << "LSB position is undefined for value 0";

        return static_cast<uint8_t>(LeastSignificantBitPosition32(value));
    }

    /// @brief Generic template to find the LSB of any unsigned integer type.
    ///
    /// Dispatches to the appropriate size-specific implementation.
    ///
    /// @tparam T An unsigned integer type.
    /// @param value The value to scan.
    /// @return The 0-based index of the least significant bit.
    // ReSharper disable once CppNotAllPathsReturnValue
    template <typename T>
        requires std::is_unsigned_v<T>
    BASALT_FORCE_INLINE T LeastSignificantBitPosition(const T value) noexcept
    {
        if constexpr (std::is_same_v<T, uint64_t>)
        {
            return LeastSignificantBitPosition64(value);
        }
        else if constexpr (std::is_same_v<T, uint32_t>)
        {
            return LeastSignificantBitPosition32(value);
        }
        else if constexpr (std::is_same_v<T, uint16_t>)
        {
            return LeastSignificantBitPosition16(value);
        }
        else if constexpr (std::is_same_v<T, uint8_t>)
        {
            return LeastSignificantBitPosition8(value);
        }
        else
        {
            static_assert(sizeof(T) == 0, "Unsupported type for LeastSignificantBitPosition");
        }
    }

    /// @brief Calculates the local bit index within a 64-bit word.
    /// @param pos The global bit index.
    /// @return The local bit index (pos % 64).
    constexpr BASALT_FORCE_INLINE uint32_t BitPosition64(const uint64_t pos) noexcept
    {
        return (pos & 63);
    }

    /// @brief Calculates the local bit index within a 32-bit word.
    /// @param pos The global bit index.
    /// @return The local bit index (pos % 32).
    constexpr BASALT_FORCE_INLINE uint32_t BitPosition32(const uint64_t pos) noexcept
    {
        return static_cast<uint32_t>(pos & 31);
    }

    /// @brief Calculates the local bit index within a 16-bit word.
    /// @param pos The global bit index.
    /// @return The local bit index (pos % 16).
    constexpr BASALT_FORCE_INLINE uint32_t BitPosition16(const uint64_t pos) noexcept
    {
        return static_cast<uint32_t>(pos & 15);
    }

    /// @brief Calculates the local bit index within an 8-bit word.
    /// @param pos The global bit index.
    /// @return The local bit index (pos % 8).
    constexpr BASALT_FORCE_INLINE uint32_t BitPosition8(const uint64_t pos) noexcept
    {
        return static_cast<uint32_t>(pos & 7);
    }

    /// @brief Generic template to calculate local bit index within a storage type T.
    ///
    /// @tparam T The underlying storage type (e.g., uint64_t, uint8_t).
    /// @param pos The global bit index.
    /// @return The 0-based index relative to the storage unit.
    // ReSharper disable once CppNotAllPathsReturnValue
    template <typename T>
        requires std::is_unsigned_v<T>
    constexpr BASALT_FORCE_INLINE uint32_t BitPosition(const uint64_t pos) noexcept
    {
        if constexpr (std::is_same_v<T, uint64_t>)
        {
            return BitPosition64(pos);
        }
        else if constexpr (std::is_same_v<T, uint32_t>)
        {
            return BitPosition32(pos);
        }
        else if constexpr (std::is_same_v<T, uint16_t>)
        {
            return BitPosition16(pos);
        }
        else if constexpr (std::is_same_v<T, uint8_t>)
        {
            return BitPosition8(pos);
        }
        else
        {
            static_assert(sizeof(T) == 0, "Unsupported type for BitPosition");
        }
    }

    /// @brief Calculates the array index for a global bit position in a uint64_t array.
    /// @param pos The global bit index.
    /// @return The index of the uint64_t element containing the bit.
    constexpr BASALT_FORCE_INLINE uint64_t BitOffset64(const uint64_t pos) noexcept
    {
        return (pos >> 6);
    }

    /// @brief Calculates the array index for a global bit position in a uint32_t array.
    /// @param pos The global bit index.
    /// @return The index of the uint32_t element containing the bit.
    constexpr BASALT_FORCE_INLINE uint64_t BitOffset32(const uint64_t pos) noexcept
    {
        return (pos >> 5);
    }

    /// @brief Calculates the array index for a global bit position in a uint16_t array.
    /// @param pos The global bit index.
    /// @return The index of the uint16_t element containing the bit.
    constexpr BASALT_FORCE_INLINE uint64_t BitOffset16(const uint64_t pos) noexcept
    {
        return (pos >> 4);
    }

    /// @brief Calculates the array index for a global bit position in a uint8_t array.
    /// @param pos The global bit index.
    /// @return The index of the uint8_t element containing the bit.
    constexpr BASALT_FORCE_INLINE uint64_t BitOffset8(const uint64_t pos) noexcept
    {
        return (pos >> 3);
    }

    /// @brief Generic template to calculate the array index for a storage type T.
    ///
    /// @tparam T The underlying storage type (e.g., uint64_t, uint8_t).
    /// @param pos The global bit index.
    /// @return The index of the element of type T containing the bit.
    // ReSharper disable once CppNotAllPathsReturnValue
    template <typename T>
        requires std::is_unsigned_v<T>
    constexpr BASALT_FORCE_INLINE uint64_t BitOffset(const uint64_t pos) noexcept
    {
        if constexpr (std::is_same_v<T, uint64_t>)
        {
            return BitOffset64(pos);
        }
        else if constexpr (std::is_same_v<T, uint32_t>)
        {
            return BitOffset32(pos);
        }
        else if constexpr (std::is_same_v<T, uint16_t>)
        {
            return BitOffset16(pos);
        }
        else if constexpr (std::is_same_v<T, uint8_t>)
        {
            return BitOffset8(pos);
        }
        else
        {
            static_assert(sizeof(T) == 0, "Unsupported type for BitOffset");
        }
    }

    /// @brief Calculates the number of uint64_t elements needed to store a given number of bits.
    /// @param size The total number of bits.
    /// @return The number of uint64_t elements required.
    constexpr BASALT_FORCE_INLINE uint64_t BitLength64(const uint64_t size) noexcept
    {
        return ((size + 63) >> 6);
    }

    /// @brief Calculates the number of uint32_t elements needed to store a given number of bits.
    /// @param size The total number of bits.
    /// @return The number of uint32_t elements required.
    constexpr BASALT_FORCE_INLINE uint64_t BitLength32(const uint64_t size) noexcept
    {
        return ((size + 31) >> 5);
    }

    /// @brief Calculates the number of uint16_t elements needed to store a given number of bits.
    /// @param size The total number of bits.
    /// @return The number of uint16_t elements required.
    constexpr BASALT_FORCE_INLINE uint64_t BitLength16(const uint64_t size) noexcept
    {
        return ((size + 15) >> 4);
    }

    /// @brief Calculates the number of uint8_t elements needed to store a given number of bits.
    /// @param size The total number of bits.
    /// @return The number of uint8_t elements required.
    constexpr BASALT_FORCE_INLINE uint64_t BitLength8(const uint64_t size) noexcept
    {
        return ((size + 7) >> 3);
    }

    /// @brief Generic template to calculate the allocation size for storage type T.
    ///
    /// @tparam T The underlying storage type (e.g., uint64_t, uint8_t).
    /// @param size The total number of bits.
    /// @return The number of elements of type T required to hold 'size' bits.
    // ReSharper disable once CppNotAllPathsReturnValue
    template <typename T>
        requires std::is_unsigned_v<T>
    constexpr BASALT_FORCE_INLINE uint64_t BitLength(const uint64_t size) noexcept
    {
        if constexpr (std::is_same_v<T, uint64_t>)
        {
            return BitLength64(size);
        }
        else if constexpr (std::is_same_v<T, uint32_t>)
        {
            return BitLength32(size);
        }
        else if constexpr (std::is_same_v<T, uint16_t>)
        {
            return BitLength16(size);
        }
        else if constexpr (std::is_same_v<T, uint8_t>)
        {
            return BitLength8(size);
        }
        else
        {
            static_assert(sizeof(T) == 0, "Unsupported type for BitLength");
        }
    }
}

#endif // BASALT_UTILS_BIT_H_
