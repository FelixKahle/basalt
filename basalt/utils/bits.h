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

#ifndef BASALT_USE_STD_BIT
#if defined(__cpp_lib_bitops)
#define BASALT_USE_STD_BIT 1
#elif defined(__cplusplus) && __cplusplus >= 202002L
#define BASALT_USE_STD_BIT 1
#else
#define BASALT_USE_STD_BIT 0
#endif // defined(__cpp_lib_bitops)
#endif // BASALT_USE_STD_BIT

// The std implementation is just superior.
#if !BASALT_USE_STD_BIT
#warning "Using custom bit operations implementation. std::bit operations are considered superior, " \
         "please consider enabling BASALT_USE_STD_BIT."
#endif // !BASALT_USE_STD_BIT

#include <limits>
#include <type_traits>
#if BASALT_USE_STD_BIT
#include <bit>
#endif // BASALT_USE_STD_BIT
#include "absl/log/check.h"
#include "basalt/base/config.h"

namespace bslt::bits
{
    // Useful bitmask constants

    constexpr uint64_t kAllBits64 = std::numeric_limits<uint64_t>::max();
    constexpr uint64_t kAllBitsButLsb64 = kAllBits64 ^ 1ULL;

    constexpr uint32_t kAllBits32 = std::numeric_limits<uint32_t>::max();
    constexpr uint32_t kAllBitsButLsb32 = kAllBits32 ^ 1U;

    constexpr uint16_t kAllBits16 = std::numeric_limits<uint16_t>::max();
    constexpr uint16_t kAllBitsButLsb16 = kAllBits16 ^ 1;

    constexpr uint8_t kAllBits8 = std::numeric_limits<uint8_t>::max();
    constexpr uint8_t kAllBitsButLsb8 = kAllBits8 ^ 1;

    /// @brief Generates a bitmask with a single bit set at the specified position for a 64-bit integer.
    ///
    /// @param pos The bit position to set (0-63).
    ///
    /// @return A 64-bit integer with only the specified bit set.
    constexpr BASALT_FORCE_INLINE uint64_t BitMask64(const uint32_t pos) noexcept
    {
        DCHECK_LT(pos, 64U) << "Shift amount must be less than 64";

        return uint64_t{1} << pos;
    }

    /// @brief Generates a bitmask with a single bit set at the specified position for a 32-bit integer.
    ///
    /// @param pos The bit position to set (0-31).
    ///
    /// @return A 32-bit integer with only the specified bit set.
    constexpr BASALT_FORCE_INLINE uint32_t BitMask32(const uint32_t pos) noexcept
    {
        DCHECK_LT(pos, 32U) << "Shift amount must be less than 32";

        return 1U << pos;
    }

    /// @brief Generates a bitmask with a single bit set at the specified position for a 16-bit integer.
    ///
    /// @param pos The bit position to set (0-15).
    ///
    /// @return A 16-bit integer with only the specified bit set.
    constexpr BASALT_FORCE_INLINE uint16_t BitMask16(const uint32_t pos) noexcept
    {
        DCHECK_LT(pos, 16U) << "Shift amount must be less than 16";

        return static_cast<uint16_t>(1U << pos);
    }

    /// @brief Generates a bitmask with a single bit set at the specified position for an 8-bit integer.
    ///
    /// @param pos The bit position to set (0-7).
    ///
    /// @return An 8-bit integer with only the specified bit set.
    constexpr BASALT_FORCE_INLINE uint8_t BitMask8(const uint32_t pos) noexcept
    {
        DCHECK_LT(pos, 8U) << "Shift amount must be less than 8";

        return static_cast<uint8_t>(1U << pos);
    }

    /// @brief Generic template to generate a bitmask for any unsigned integer type.
    ///
    /// @tparam T An unsigned integer type.
    ///
    /// @param pos The bit position to set.
    ///
    /// @return A value of type T with only the specified bit set.
    // ReSharper disable once CppNotAllPathsReturnValue
    template <typename T>
        requires std::is_unsigned_v<T>
    constexpr BASALT_FORCE_INLINE T BitMask(const uint32_t pos) noexcept
    {
        if constexpr (std::is_same_v<T, uint64_t>)
        {
            return BitMask64(pos);
        }
        else if constexpr (std::is_same_v<T, uint32_t>)
        {
            return BitMask32(pos);
        }
        else if constexpr (std::is_same_v<T, uint16_t>)
        {
            return BitMask16(pos);
        }
        else if constexpr (std::is_same_v<T, uint8_t>)
        {
            return BitMask8(pos);
        }
        else
        {
            static_assert(sizeof(T) == 0, "Unsupported type for BitMask");
        }
    }

    /// @brief Generates a bitmask with bits set between the start and end positions (inclusive) for a 64-bit integer.
    ///
    /// @param start The starting bit position to set (0-63).
    /// @param end The ending bit position to set (0-63).
    ///
    /// @return A 64-bit integer with bits [start, end] set and others cleared.
    constexpr BASALT_FORCE_INLINE uint64_t BitMaskRange64(const uint32_t start, const uint32_t end) noexcept
    {
        DCHECK_LE(start, 63) << "Start position must be less than 64";
        DCHECK_LE(end, 63) << "End position must be less than 64";
        DCHECK_LE(start, end) << "Start position must be less than or equal to end position";

        return (kAllBits64 << start) ^ ((kAllBits64 - 1) << end);
    }

    /// @brief Generates a bitmask with bits set between the start and end positions (inclusive) for a 32-bit integer.
    ///
    /// @param start The starting bit position to set (0-31).
    /// @param end The ending bit position to set (0-31).
    ///
    /// @return A 32-bit integer with bits [start, end] set and others cleared.
    constexpr BASALT_FORCE_INLINE uint32_t BitMaskRange32(const uint32_t start, const uint32_t end) noexcept
    {
        DCHECK_LE(start, 31) << "Start position must be less than 32";
        DCHECK_LE(end, 31) << "End position must be less than 32";
        DCHECK_LE(start, end) << "Start position must be less than or equal to end position";

        return (kAllBits32 << start) ^ ((kAllBits32 - 1) << end);
    }

    /// @brief Generates a bitmask with bits set between the start and end positions (inclusive) for a 16-bit integer.
    ///
    /// @param start The starting bit position to set (0-15).
    /// @param end The ending bit position to set (0-15).
    ///
    /// @return A 16-bit integer with bits [start, end] set and others cleared.
    constexpr BASALT_FORCE_INLINE uint16_t BitMaskRange16(const uint32_t start, const uint32_t end) noexcept
    {
        DCHECK_LE(start, 15) << "Start position must be less than 16";
        DCHECK_LE(end, 15) << "End position must be less than 16";
        DCHECK_LE(start, end) << "Start position must be less than or equal to end position";

        return static_cast<uint16_t>((kAllBits16 << start) ^ ((kAllBits16 - 1) << end));
    }

    /// @brief Generates a bitmask with bits set between the start and end positions (inclusive) for an 8-bit integer.
    ///
    /// @param start The starting bit position to set (0-7).
    /// @param end The ending bit position to set (0-7).
    ///
    /// @return An 8-bit integer with bits [start, end] set and others cleared.
    constexpr BASALT_FORCE_INLINE uint8_t BitMaskRange8(const uint32_t start, const uint32_t end) noexcept
    {
        DCHECK_LE(start, 7) << "Start position must be less than 8";
        DCHECK_LE(end, 7) << "End position must be less than 8";
        DCHECK_LE(start, end) << "Start position must be less than or equal to end position";

        return static_cast<uint8_t>((kAllBits8 << start) ^ ((kAllBits8 - 1) << end));
    }

    /// @brief Generic template to generate a bitmask range for any unsigned integer type.
    ///
    /// @tparam T An unsigned integer type.
    ///
    /// @param start The starting bit position to set.
    /// @param end The ending bit position to set.
    ///
    /// @return A value of type T with bits [start, end] set and others cleared.
    // ReSharper disable once CppNotAllPathsReturnValue
    template <typename T>
        requires std::is_unsigned_v<T>
    constexpr BASALT_FORCE_INLINE T BitMaskRange(const uint32_t start, const uint32_t end) noexcept
    {
        if constexpr (std::is_same_v<T, uint64_t>)
        {
            return BitMaskRange64(start, end);
        }
        else if constexpr (std::is_same_v<T, uint32_t>)
        {
            return BitMaskRange32(start, end);
        }
        else if constexpr (std::is_same_v<T, uint16_t>)
        {
            return BitMaskRange16(start, end);
        }
        else if constexpr (std::is_same_v<T, uint8_t>)
        {
            return BitMaskRange8(start, end);
        }
        else
        {
            static_assert(sizeof(T) == 0, "Unsupported type for BitMaskRange");
        }
    }

    /// @brief Generates an inverse bitmask with all bits set except the one at the specified position for a 64-bit integer.
    ///
    /// @param pos The bit position to clear (0-63).
    ///
    /// @return A 64-bit integer with all bits set except the specified bit.
    constexpr BASALT_FORCE_INLINE uint64_t InverseBitMask64(const uint32_t pos) noexcept
    {
        DCHECK_LT(pos, 64U) << "Shift amount must be less than 64";

        return ~(uint64_t{1} << pos);
    }

    /// @brief Generates an inverse bitmask with all bits set except the one at the specified position for a 32-bit integer.
    ///
    /// @param pos The bit position to clear (0-31).
    ///
    /// @return A 32-bit integer with all bits set except the specified bit.
    constexpr BASALT_FORCE_INLINE uint32_t InverseBitMask32(const uint32_t pos) noexcept
    {
        DCHECK_LT(pos, 32U) << "Shift amount must be less than 32";

        return ~(1U << pos);
    }

    /// @brief Generates an inverse bitmask with all bits set except the one at the specified position for a 16-bit integer.
    ///
    /// @param pos The bit position to clear (0-15).
    ///
    /// @return A 16-bit integer with all bits set except the specified bit.
    constexpr BASALT_FORCE_INLINE uint16_t InverseBitMask16(const uint32_t pos) noexcept
    {
        DCHECK_LT(pos, 16U) << "Shift amount must be less than 16";

        return static_cast<uint16_t>(~(1U << pos));
    }

    /// @brief Generates an inverse bitmask with all bits set except the one at the specified position for an 8-bit integer.
    ///
    /// @param pos The bit position to clear (0-7).
    ///
    /// @return An 8-bit integer with all bits set except the specified bit.
    constexpr BASALT_FORCE_INLINE uint8_t InverseBitMask8(const uint32_t pos) noexcept
    {
        DCHECK_LT(pos, 8U) << "Shift amount must be less than 8";

        return static_cast<uint8_t>(~(1U << pos));
    }

    /// @brief Generic template to generate an inverse bitmask for any unsigned integer type.
    ///
    /// @tparam T An unsigned integer type.
    ///
    /// @param pos The bit position to clear.
    ///
    /// @return A value of type T with all bits set except the specified bit.
    // ReSharper disable once CppNotAllPathsReturnValue
    template <typename T>
        requires std::is_unsigned_v<T>
    constexpr BASALT_FORCE_INLINE T InverseBitMask(const uint32_t pos) noexcept
    {
        if constexpr (std::is_same_v<T, uint64_t>)
        {
            return InverseBitMask64(pos);
        }
        else if constexpr (std::is_same_v<T, uint32_t>)
        {
            return InverseBitMask32(pos);
        }
        else if constexpr (std::is_same_v<T, uint16_t>)
        {
            return InverseBitMask16(pos);
        }
        else if constexpr (std::is_same_v<T, uint8_t>)
        {
            return InverseBitMask8(pos);
        }
        else
        {
            static_assert(sizeof(T) == 0, "Unsupported type for InverseBitMask");
        }
    }

    /// @brief Generates an inverse bitmask with bits cleared between the start and end positions (inclusive) for a 64-bit integer.
    ///
    /// @param start The starting bit position to clear (0-63).
    /// @param end The ending bit position to clear (0-63).
    ///
    /// @return A 64-bit integer with bits [start, end] cleared and others set.
    constexpr BASALT_FORCE_INLINE uint64_t InverseBitMaskRange64(const uint32_t start, const uint32_t end) noexcept
    {
        DCHECK_LE(start, 63) << "Start position must be less than 64";
        DCHECK_LE(end, 63) << "End position must be less than 64";
        DCHECK_LE(start, end) << "Start position must be less than or equal to end position";

        // Calculate the positive mask and invert it
        return ~((kAllBits64 << start) ^ ((kAllBits64 - 1) << end));
    }

    /// @brief Generates an inverse bitmask with bits cleared between the start and end positions (inclusive) for a 32-bit integer.
    ///
    /// @param start The starting bit position to clear (0-31).
    /// @param end The ending bit position to clear (0-31).
    ///
    /// @return A 32-bit integer with bits [start, end] cleared and others set.
    constexpr BASALT_FORCE_INLINE uint32_t InverseBitMaskRange32(const uint32_t start, const uint32_t end) noexcept
    {
        DCHECK_LE(start, 31) << "Start position must be less than 32";
        DCHECK_LE(end, 31) << "End position must be less than 32";
        DCHECK_LE(start, end) << "Start position must be less than or equal to end position";

        return ~((kAllBits32 << start) ^ ((kAllBits32 - 1) << end));
    }

    /// @brief Generates an inverse bitmask with bits cleared between the start and end positions (inclusive) for a 16-bit integer.
    ///
    /// @param start The starting bit position to clear (0-15).
    /// @param end The ending bit position to clear (0-15).
    ///
    /// @return A 16-bit integer with bits [start, end] cleared and others set.
    constexpr BASALT_FORCE_INLINE uint16_t InverseBitMaskRange16(const uint32_t start, const uint32_t end) noexcept
    {
        DCHECK_LE(start, 15) << "Start position must be less than 16";
        DCHECK_LE(end, 15) << "End position must be less than 16";
        DCHECK_LE(start, end) << "Start position must be less than or equal to end position";

        return static_cast<uint16_t>(~((kAllBits16 << start) ^ ((kAllBits16 - 1) << end)));
    }

    /// @brief Generates an inverse bitmask with bits cleared between the start and end positions (inclusive) for an 8-bit integer.
    ///
    /// @param start The starting bit position to clear (0-7).
    /// @param end The ending bit position to clear (0-7).
    ///
    /// @return An 8-bit integer with bits [start, end] cleared and others set.
    constexpr BASALT_FORCE_INLINE uint8_t InverseBitMaskRange8(const uint32_t start, const uint32_t end) noexcept
    {
        DCHECK_LE(start, 7) << "Start position must be less than 8";
        DCHECK_LE(end, 7) << "End position must be less than 8";
        DCHECK_LE(start, end) << "Start position must be less than or equal to end position";

        return static_cast<uint8_t>(~((kAllBits8 << start) ^ ((kAllBits8 - 1) << end)));
    }

    /// @brief Generic template to generate an inverse bitmask range for any unsigned integer type.
    ///
    /// @tparam T An unsigned integer type.
    ///
    /// @param start The starting bit position to clear.
    /// @param end The ending bit position to clear.
    ///
    /// @return A value of type T with bits [start, end] cleared and others set.
    // ReSharper disable once CppNotAllPathsReturnValue
    template <typename T>
        requires std::is_unsigned_v<T>
    constexpr BASALT_FORCE_INLINE T InverseBitMaskRange(const uint32_t start, const uint32_t end) noexcept
    {
        if constexpr (std::is_same_v<T, uint64_t>)
        {
            return InverseBitMaskRange64(start, end);
        }
        else if constexpr (std::is_same_v<T, uint32_t>)
        {
            return InverseBitMaskRange32(start, end);
        }
        else if constexpr (std::is_same_v<T, uint16_t>)
        {
            return InverseBitMaskRange16(start, end);
        }
        else if constexpr (std::is_same_v<T, uint8_t>)
        {
            return InverseBitMaskRange8(start, end);
        }
        else
        {
            static_assert(sizeof(T) == 0, "Unsupported type for InverseBitMaskRange");
        }
    }

    /// @brief Calculates the population count (number of set bits) in a 64-bit integer.
    ///
    /// Uses a SWAR (SIMD Within A Register) algorithm for constant-time execution.
    ///
    /// @param n The value to count bits in.
    ///
    /// @return The number of bits set to 1.
    constexpr BASALT_FORCE_INLINE uint64_t BitCount64(uint64_t n) noexcept
    {
#if BASALT_USE_STD_BIT
        return static_cast<uint64_t>(std::popcount(n));
#else
        constexpr auto m1 = uint64_t{0x5555555555555555};
        constexpr auto m2 = uint64_t{0x3333333333333333};
        constexpr auto m4 = uint64_t{0x0F0F0F0F0F0F0F0F};
        constexpr auto h01 = uint64_t{0x0101010101010101};
        n -= (n >> 1) & m1;
        n = (n & m2) + ((n >> 2) & m2);
        n = (n + (n >> 4)) & m4;
        n = (n * h01) >> 56;
        return n;
#endif
    }

    /// @brief Calculates the population count (number of set bits) in a 32-bit integer.
    ///
    /// @param n The value to count bits in.
    ///
    /// @return The number of bits set to 1.
    constexpr BASALT_FORCE_INLINE uint32_t BitCount32(uint32_t n) noexcept
    {
#if BASALT_USE_STD_BIT
        return static_cast<uint32_t>(std::popcount(n));
#else
        n -= (n >> 1) & 0x55555555UL;
        n = (n & 0x33333333) + ((n >> 2) & 0x33333333UL);
        n = (n + (n >> 4)) & 0x0F0F0F0FUL;
        n = n + (n >> 8);
        n = n + (n >> 16);
        return n & 0x0000003FUL;
#endif // BASALT_USE_STD_BIT
    }

    /// @brief Calculates the population count (number of set bits) in a 16-bit integer.
    ///
    /// @param n The value to count bits in.
    ///
    /// @return The number of bits set to 1.
    constexpr BASALT_FORCE_INLINE uint16_t BitCount16(uint16_t n) noexcept
    {
#if BASALT_USE_STD_BIT
        return static_cast<uint16_t>(std::popcount(n));
#else
        n -= (n >> 1) & 0x5555U;
        n = (n & 0x3333U) + ((n >> 2) & 0x3333U);
        n = (n + (n >> 4)) & 0x0F0FU;
        return static_cast<uint16_t>((n + (n >> 8)) & 0x001FU);
#endif // BASALT_USE_STD_BIT
    }

    /// @brief Calculates the population count (number of set bits) in an 8-bit integer.
    ///
    /// @param n The value to count bits in.
    ///
    /// @return The number of bits set to 1.
    constexpr BASALT_FORCE_INLINE uint8_t BitCount8(uint8_t n) noexcept
    {
#if BASALT_USE_STD_BIT
        return static_cast<uint8_t>(std::popcount(n));
#else
        n = static_cast<uint8_t>(n - ((n >> 1) & 0x55U));
        n = static_cast<uint8_t>((n & 0x33U) + ((n >> 2) & 0x33U));
        return static_cast<uint8_t>((n + (n >> 4)) & 0x0FU);
#endif // BASALT_USE_STD_BIT
    }

    /// @brief Generic template to calculate the population count for any unsigned integer type.
    ///
    /// @tparam T An unsigned integer type.
    ///
    /// @param n The value to count bits in.
    ///
    /// @return The number of bits set to 1.
    // ReSharper disable once CppNotAllPathsReturnValue
    template <typename T>
    constexpr BASALT_FORCE_INLINE T BitCount(const T n) noexcept
    {
        if constexpr (std::is_same_v<T, uint64_t>)
        {
            return BitCount64(n);
        }
        else if constexpr (std::is_same_v<T, uint32_t>)
        {
            return BitCount32(n);
        }
        else if constexpr (std::is_same_v<T, uint16_t>)
        {
            return BitCount16(n);
        }
        else if constexpr (std::is_same_v<T, uint8_t>)
        {
            return BitCount8(n);
        }
        else
        {
            static_assert(sizeof(T) == 0, "Unsupported type for BitCount");
        }
    }

    /// @brief Isolates the least significant bit of a 64-bit integer.
    ///
    /// If n is 0, returns 0. Otherwise, returns a value with only the least significant bit of n set.
    ///
    /// @param n The value to process.
    ///
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
    ///
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
    ///
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
    ///
    /// @return A value with only the least significant bit of n set.
    constexpr BASALT_FORCE_INLINE uint8_t LeastSignificantBitWord8(const uint8_t n) noexcept
    {
        return n & static_cast<uint8_t>(~(n - 1));
    }

    /// @brief Generic template to isolate the least significant bit of any unsigned integer type.
    ///
    /// @param n The value to process.
    ///
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
    /// The De Bruijn sequence method provides a fast way to determine the position of the least significant bit,
    /// for more details see: <a href="https://en.wikipedia.org/wiki/De_Bruijn_sequence">De Bruijn</a>.
    ///
    /// @param n The 64-bit integer to check. Must not be 0.
    ///
    /// @return The 0-based index of the least significant bit (0-63).
    ///
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
    ///
    /// @return The 0-based index of the least significant bit (0-63).
    ///
    /// @note The behavior is undefined if value is 0.
    constexpr BASALT_FORCE_INLINE uint64_t LeastSignificantBitPosition64(const uint64_t value) noexcept
    {
        DCHECK_NE(value, 0ULL) << "LSB position is undefined for value 0";

#if BASALT_USE_STD_BIT
        return static_cast<uint64_t>(std::countr_zero(value));
#else
        return LeastSignificantBitPosition64DeBruijn(value);
#endif // BASALT_USE_STD_BIT
    }

    /// @brief Returns the index of the least significant bit set in a 32-bit integer.
    ///
    /// @param value The value to scan. Must not be 0.
    ///
    /// @return The 0-based index of the least significant bit (0-31).
    ///
    /// @note The behavior is undefined if value is 0.
    constexpr BASALT_FORCE_INLINE uint32_t LeastSignificantBitPosition32(const uint32_t value) noexcept
    {
        DCHECK_NE(value, 0U) << "LSB position is undefined for value 0";

#if BASALT_USE_STD_BIT
        return static_cast<uint32_t>(std::countr_zero(value));
#else
        return static_cast<uint32_t>(LeastSignificantBitPosition64(static_cast<uint64_t>(value)));
#endif // BASALT_USE_STD_BIT
    }

    /// @brief Returns the index of the least significant bit set in a 16-bit integer.
    ///
    /// @param value The value to scan. Must not be 0.
    ///
    /// @return The 0-based index of the least significant bit (0-15).
    constexpr BASALT_FORCE_INLINE uint16_t LeastSignificantBitPosition16(const uint16_t value) noexcept
    {
        DCHECK_NE(value, 0U) << "LSB position is undefined for value 0";

#if BASALT_USE_STD_BIT
        return static_cast<uint16_t>(std::countr_zero(value));
#else
        return static_cast<uint16_t>(LeastSignificantBitPosition32(value));
#endif // BASALT_USE_STD_BIT
    }

    /// @brief Returns the index of the least significant bit set in an 8-bit integer.
    ///
    /// @param value The value to scan. Must not be 0.
    ///
    /// @return The 0-based index of the least significant bit (0-7).
    constexpr BASALT_FORCE_INLINE uint8_t LeastSignificantBitPosition8(const uint8_t value) noexcept
    {
        DCHECK_NE(value, 0U) << "LSB position is undefined for value 0";
#if BASALT_USE_STD_BIT
        return static_cast<uint8_t>(std::countr_zero(value));
#else
        return static_cast<uint8_t>(LeastSignificantBitPosition32(value));
#endif // BASALT_USE_STD_BIT
    }

    /// @brief Generic template to find the LSB of any unsigned integer type.
    ///
    /// Dispatches to the appropriate size-specific implementation.
    ///
    /// @tparam T An unsigned integer type.
    ///
    /// @param value The value to scan.
    ///
    /// @return The 0-based index of the least significant bit.
    // ReSharper disable once CppNotAllPathsReturnValue
    template <typename T>
        requires std::is_unsigned_v<T>
    constexpr BASALT_FORCE_INLINE T LeastSignificantBitPosition(const T value) noexcept
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

    /// @brief Isolates the most significant bit of a 64-bit integer.
    ///
    /// @param n The value to check.
    ///
    /// @return A value with only the most significant bit of n set.
    constexpr BASALT_FORCE_INLINE uint64_t MostSignificantBitWord64(uint64_t n) noexcept
    {
        DCHECK_NE(n, 0ULL) << "MSB word is undefined for value 0";

#if BASALT_USE_STD_BIT
        return std::bit_floor(n);
#else
        n |= n >> 1;
        n |= n >> 2;
        n |= n >> 4;
        n |= n >> 8;
        n |= n >> 16;
        n |= n >> 32;
        return n - (n >> 1);
#endif
    }

    /// @brief Isolates the most significant bit of a 32-bit integer.
    ///
    /// @param n The value to check.
    ///
    /// @return A value with only the most significant bit of n set.
    constexpr BASALT_FORCE_INLINE uint32_t MostSignificantBitWord32(uint32_t n) noexcept
    {
        DCHECK_NE(n, 0U) << "MSB word is undefined for value 0";

#if BASALT_USE_STD_BIT
        return std::bit_floor(n);
#else
        n |= n >> 1;
        n |= n >> 2;
        n |= n >> 4;
        n |= n >> 8;
        n |= n >> 16;
        return n - (n >> 1);
#endif
    }

    /// @brief Isolates the most significant bit of a 16-bit integer.
    ///
    /// @param n The value to check.
    ///
    /// @return A value with only the most significant bit of n set.
    constexpr BASALT_FORCE_INLINE uint16_t MostSignificantBitWord16(uint16_t n) noexcept
    {
        DCHECK_NE(n, 0U) << "MSB word is undefined for value 0";

#if BASALT_USE_STD_BIT
        return std::bit_floor(n);
#else
        n = static_cast<uint16_t>(n | (n >> 1));
        n = static_cast<uint16_t>(n | (n >> 2));
        n = static_cast<uint16_t>(n | (n >> 4));
        n = static_cast<uint16_t>(n | (n >> 8));
        return static_cast<uint16_t>(n - (n >> 1));
#endif
    }

    /// @brief Isolates the most significant bit of an 8-bit integer.
    ///
    /// @param n The value to check.
    ///
    /// @return A value with only the most significant bit of n set.
    constexpr BASALT_FORCE_INLINE uint8_t MostSignificantBitWord8(uint8_t n) noexcept
    {
        DCHECK_NE(n, 0U) << "MSB word is undefined for value 0";

#if BASALT_USE_STD_BIT
        return std::bit_floor(n);
#else
        n = static_cast<uint8_t>(n | (n >> 1));
        n = static_cast<uint8_t>(n | (n >> 2));
        n = static_cast<uint8_t>(n | (n >> 4));
        return static_cast<uint8_t>(n - (n >> 1));
#endif
    }

    /// @brief Generic template to isolate the most significant bit of any unsigned integer type.
    ///
    /// @tparam T An unsigned integer type.
    ///
    /// @param n The value to process.
    ///
    /// @return A value of type T with only the most significant bit of n set.
    // ReSharper disable once CppNotAllPathsReturnValue
    template <typename T>
        requires std::is_unsigned_v<T>
    constexpr BASALT_FORCE_INLINE T MostSignificantBitWord(const T n) noexcept
    {
        if constexpr (std::is_same_v<T, uint64_t>)
        {
            return MostSignificantBitWord64(n);
        }
        else if constexpr (std::is_same_v<T, uint32_t>)
        {
            return MostSignificantBitWord32(n);
        }
        else if constexpr (std::is_same_v<T, uint16_t>)
        {
            return MostSignificantBitWord16(n);
        }
        else if constexpr (std::is_same_v<T, uint8_t>)
        {
            return MostSignificantBitWord8(n);
        }
        else
        {
            static_assert(sizeof(T) == 0, "Unsupported type for MostSignificantBitWord");
        }
    }


    /// @brief Calculates the index of the most significant bit in a 64-bit integer.
    ///
    /// @param n The value to check.
    ///
    /// @return The 0-based index of the most significant bit (0-63).
    constexpr BASALT_FORCE_INLINE uint64_t MostSignificantBitPosition64(uint64_t n) noexcept
    {
        DCHECK_NE(n, 0ULL) << "MSB position is undefined for value 0";

#if BASALT_USE_STD_BIT
        return static_cast<uint64_t>(std::bit_width(n) - 1);
#else
        int b = 0;
        if (0 != (n & (kAllBits64 << (1 << 5))))
        {
            b |= (1 << 5);
            n >>= (1 << 5);
        }
        if (0 != (n & (kAllBits64 << (1 << 4))))
        {
            b |= (1 << 4);
            n >>= (1 << 4);
        }
        if (0 != (n & (kAllBits64 << (1 << 3))))
        {
            b |= (1 << 3);
            n >>= (1 << 3);
        }
        if (0 != (n & (kAllBits64 << (1 << 2))))
        {
            b |= (1 << 2);
            n >>= (1 << 2);
        }
        if (0 != (n & (kAllBits64 << (1 << 1))))
        {
            b |= (1 << 1);
            n >>= (1 << 1);
        }
        if (0 != (n & (kAllBits64 << (1 << 0))))
        {
            b |= (1 << 0);
        }
        return b;
#endif
    }

    /// @brief Calculates the index of the most significant bit in a 32-bit integer.
    ///
    /// @param n The value to check.
    ///
    /// @return The 0-based index of the most significant bit (0-31).
    constexpr BASALT_FORCE_INLINE uint32_t MostSignificantBitPosition32(uint32_t n) noexcept
    {
        DCHECK_NE(n, 0U) << "MSB position is undefined for value 0";

#if BASALT_USE_STD_BIT
        return static_cast<uint32_t>(std::bit_width(n) - 1);
#else
        int b = 0;
        if (0 != (n & (kAllBits32 << (1 << 4))))
        {
            b |= (1 << 4);
            n >>= (1 << 4);
        }
        if (0 != (n & (kAllBits32 << (1 << 3))))
        {
            b |= (1 << 3);
            n >>= (1 << 3);
        }
        if (0 != (n & (kAllBits32 << (1 << 2))))
        {
            b |= (1 << 2);
            n >>= (1 << 2);
        }
        if (0 != (n & (kAllBits32 << (1 << 1))))
        {
            b |= (1 << 1);
            n >>= (1 << 1);
        }
        if (0 != (n & (kAllBits32 << (1 << 0))))
        {
            b |= (1 << 0);
        }
        return b;
#endif
    }

    /// @brief Returns the index of the most significant bit set in a 16-bit integer.
    ///
    /// @param n The value to scan. Must not be 0.
    ///
    /// @return The 0-based index of the most significant bit (0-15).
    constexpr BASALT_FORCE_INLINE uint16_t MostSignificantBitPosition16(uint16_t n) noexcept
    {
        DCHECK_NE(n, 0U) << "MSB position is undefined for value 0";

#if BASALT_USE_STD_BIT
        return static_cast<uint16_t>(std::bit_width(n) - 1);
#else
        int b = 0;
        if (0 != (n & (kAllBits16 << (1 << 3)))) // Check top 8 bits
        {
            b |= (1 << 3);
            n >>= (1 << 3);
        }
        if (0 != (n & (kAllBits16 << (1 << 2)))) // Check top 4 bits
        {
            b |= (1 << 2);
            n >>= (1 << 2);
        }
        if (0 != (n & (kAllBits16 << (1 << 1)))) // Check top 2 bits
        {
            b |= (1 << 1);
            n >>= (1 << 1);
        }
        if (0 != (n & (kAllBits16 << (1 << 0)))) // Check top 1 bit
        {
            b |= (1 << 0);
        }
        return static_cast<uint16_t>(b);
#endif
    }

    /// @brief Returns the index of the most significant bit set in an 8-bit integer.
    ///
    /// @param n The value to scan. Must not be 0.
    ///
    /// @return The 0-based index of the most significant bit (0-7).
    constexpr BASALT_FORCE_INLINE uint8_t MostSignificantBitPosition8(uint8_t n) noexcept
    {
        DCHECK_NE(n, 0U) << "MSB position is undefined for value 0";

#if BASALT_USE_STD_BIT
        return static_cast<uint8_t>(std::bit_width(n) - 1);
#else
        int b = 0;
        if (0 != (n & (kAllBits8 << (1 << 2)))) // Check top 4 bits
        {
            b |= (1 << 2);
            n >>= (1 << 2);
        }
        if (0 != (n & (kAllBits8 << (1 << 1)))) // Check top 2 bits
        {
            b |= (1 << 1);
            n >>= (1 << 1);
        }
        if (0 != (n & (kAllBits8 << (1 << 0)))) // Check top 1 bit
        {
            b |= (1 << 0);
        }
        return static_cast<uint8_t>(b);
#endif
    }

    /// @brief Generic template to find the MSB index of any unsigned integer type.
    ///
    /// Dispatches to the appropriate size-specific implementation.
    ///
    /// @tparam T An unsigned integer type.
    ///
    /// @param value The value to scan.
    ///
    /// @return The 0-based index of the most significant bit.
    // ReSharper disable once CppNotAllPathsReturnValue
    template <typename T>
        requires std::is_unsigned_v<T>
    BASALT_FORCE_INLINE T MostSignificantBitPosition(const T value) noexcept
    {
        if constexpr (std::is_same_v<T, uint64_t>)
        {
            return MostSignificantBitPosition64(value);
        }
        else if constexpr (std::is_same_v<T, uint32_t>)
        {
            return MostSignificantBitPosition32(value);
        }
        else if constexpr (std::is_same_v<T, uint16_t>)
        {
            return MostSignificantBitPosition16(value);
        }
        else if constexpr (std::is_same_v<T, uint8_t>)
        {
            return MostSignificantBitPosition8(value);
        }
        else
        {
            static_assert(sizeof(T) == 0, "Unsupported type for MostSignificantBitPosition");
        }
    }

    /// @brief Calculates the local bit index within a 64-bit word.
    ///
    /// @param pos The global bit index.
    ///
    /// @return The local bit index (pos % 64).
    constexpr BASALT_FORCE_INLINE uint32_t BitPosition64(const uint64_t pos) noexcept
    {
        return (pos & 63);
    }

    /// @brief Calculates the local bit index within a 32-bit word.
    ///
    /// @param pos The global bit index.
    ///
    /// @return The local bit index (pos % 32).
    constexpr BASALT_FORCE_INLINE uint32_t BitPosition32(const uint64_t pos) noexcept
    {
        return static_cast<uint32_t>(pos & 31);
    }

    /// @brief Calculates the local bit index within a 16-bit word.
    ///
    /// @param pos The global bit index.
    ///
    /// @return The local bit index (pos % 16).
    constexpr BASALT_FORCE_INLINE uint32_t BitPosition16(const uint64_t pos) noexcept
    {
        return static_cast<uint32_t>(pos & 15);
    }

    /// @brief Calculates the local bit index within an 8-bit word.
    ///
    /// @param pos The global bit index.
    ///
    /// @return The local bit index (pos % 8).
    constexpr BASALT_FORCE_INLINE uint32_t BitPosition8(const uint64_t pos) noexcept
    {
        return static_cast<uint32_t>(pos & 7);
    }

    /// @brief Generic template to calculate local bit index within a storage type T.
    ///
    /// @tparam T The underlying storage type (e.g., uint64_t, uint8_t).
    ///
    /// @param pos The global bit index.
    ///
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
    ///
    /// @param pos The global bit index.
    ///
    /// @return The index of the uint64_t element containing the bit.
    constexpr BASALT_FORCE_INLINE uint64_t BitOffset64(const uint64_t pos) noexcept
    {
        return (pos >> 6);
    }

    /// @brief Calculates the array index for a global bit position in a uint32_t array.
    ///
    /// @param pos The global bit index.
    ///
    /// @return The index of the uint32_t element containing the bit.
    constexpr BASALT_FORCE_INLINE uint64_t BitOffset32(const uint64_t pos) noexcept
    {
        return (pos >> 5);
    }

    /// @brief Calculates the array index for a global bit position in a uint16_t array.
    ///
    /// @param pos The global bit index.
    ///
    /// @return The index of the uint16_t element containing the bit.
    constexpr BASALT_FORCE_INLINE uint64_t BitOffset16(const uint64_t pos) noexcept
    {
        return (pos >> 4);
    }

    /// @brief Calculates the array index for a global bit position in a uint8_t array.
    ///
    /// @param pos The global bit index.
    ///
    /// @return The index of the uint8_t element containing the bit.
    constexpr BASALT_FORCE_INLINE uint64_t BitOffset8(const uint64_t pos) noexcept
    {
        return (pos >> 3);
    }

    /// @brief Generic template to calculate the array index for a storage type T.
    ///
    /// @tparam T The underlying storage type (e.g., uint64_t, uint8_t).
    ///
    /// @param pos The global bit index.
    ///
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
    ///
    /// @param size The total number of bits.
    ///
    /// @return The number of uint64_t elements required.
    constexpr BASALT_FORCE_INLINE uint64_t BitLength64(const uint64_t size) noexcept
    {
        return ((size + 63) >> 6);
    }

    /// @brief Calculates the number of uint32_t elements needed to store a given number of bits.
    ///
    /// @param size The total number of bits.
    ///
    /// @return The number of uint32_t elements required.
    constexpr BASALT_FORCE_INLINE uint64_t BitLength32(const uint64_t size) noexcept
    {
        return ((size + 31) >> 5);
    }

    /// @brief Calculates the number of uint16_t elements needed to store a given number of bits.
    ///
    /// @param size The total number of bits.
    ///
    /// @return The number of uint16_t elements required.
    constexpr BASALT_FORCE_INLINE uint64_t BitLength16(const uint64_t size) noexcept
    {
        return ((size + 15) >> 4);
    }

    /// @brief Calculates the number of uint8_t elements needed to store a given number of bits.
    ///
    /// @param size The total number of bits.
    ///
    /// @return The number of uint8_t elements required.
    constexpr BASALT_FORCE_INLINE uint64_t BitLength8(const uint64_t size) noexcept
    {
        return ((size + 7) >> 3);
    }

    /// @brief Generic template to calculate the allocation size for storage type T.
    ///
    /// @tparam T The underlying storage type (e.g., uint64_t, uint8_t).
    ///
    /// @param size The total number of bits.
    ///
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

    /// @brief Creates a bitmask with bits set from position s to the most significant bit for a 64-bit integer.
    ///
    /// @param s The starting bit position (0-63).
    ///
    /// @return A bitmask with bits set from position s to the most significant bit.
    constexpr BASALT_FORCE_INLINE uint64_t IntervalUp64(const uint64_t s)
    {
        DCHECK_LE(s, 63) << "Shift exceeds bit width";

        return kAllBits64 << s;
    }

    /// @brief Creates a bitmask with bits set from position s to the most significant bit for a 32-bit integer.
    ///
    /// @param s The starting bit position (0-31).
    ///
    /// @return A bitmask with bits set from position s to the most significant bit.
    constexpr BASALT_FORCE_INLINE uint32_t IntervalUp32(const uint32_t s)
    {
        DCHECK_LE(s, 31) << "Shift exceeds bit width";

        return kAllBits32 << s;
    }

    /// @brief Creates a bitmask with bits set from position s to the most significant bit for a 16-bit integer.
    ///
    /// @param s The starting bit position (0-15).
    ///
    /// @return A bitmask with bits set from position s to the most significant bit.
    constexpr BASALT_FORCE_INLINE uint16_t IntervalUp16(const uint16_t s)
    {
        DCHECK_LE(s, 15) << "Shift exceeds bit width";

        return static_cast<uint16_t>(kAllBits16 << s);
    }

    /// @brief Creates a bitmask with bits set from position s to the most significant bit for an 8-bit integer.
    ///
    /// @param s The starting bit position (0-7).
    ///
    /// @return A bitmask with bits set from position s to the most significant bit.
    constexpr BASALT_FORCE_INLINE uint8_t IntervalUp8(const uint8_t s)
    {
        DCHECK_LE(s, 7) << "Shift exceeds bit width";

        return static_cast<uint8_t>(kAllBits8 << s);
    }

    /// @brief Generic template to create a bitmask with bits set from position s to the most significant bit.
    ///
    /// @tparam T An unsigned integer type.
    ///
    /// @param s The starting bit position.
    ///
    /// @return A bitmask of type T with bits set from position s to the most significant bit.
    // ReSharper disable once CppNotAllPathsReturnValue
    template <typename T>
        requires std::is_unsigned_v<T>
    constexpr BASALT_FORCE_INLINE T IntervalUp(const T s)
    {
        if constexpr (std::is_same_v<T, uint64_t>)
        {
            return IntervalUp64(s);
        }
        else if constexpr (std::is_same_v<T, uint32_t>)
        {
            return IntervalUp32(s);
        }
        else if constexpr (std::is_same_v<T, uint16_t>)
        {
            return IntervalUp16(s);
        }
        else if constexpr (std::is_same_v<T, uint8_t>)
        {
            return IntervalUp8(s);
        }
        else
        {
            static_assert(sizeof(T) == 0, "Unsupported type for IntervalUp");
        }
    }

    /// @brief Creates a bitmask with bits set from the least significant bit up to position s for a 64-bit integer.
    ///
    /// @param s The ending bit position (0-63).
    ///
    /// @return A bitmask with bits set from the least significant bit up to position s.
    constexpr BASALT_FORCE_INLINE uint64_t IntervalDown64(const uint64_t s)
    {
        DCHECK_LE(s, 63) << "Shift exceeds bit width";
        return kAllBits64 >> (63 - s);
    }

    /// @brief Creates a bitmask with bits set from the least significant bit up to position s for a 32-bit integer.
    ///
    /// @param s The ending bit position (0-31).
    ///
    /// @return A bitmask with bits set from the least significant bit up to position s.
    constexpr BASALT_FORCE_INLINE uint32_t IntervalDown32(const uint32_t s)
    {
        DCHECK_LE(s, 31) << "Shift exceeds bit width";
        return kAllBits32 >> (31 - s);
    }

    /// @brief Creates a bitmask with bits set from the least significant bit up to position s for a 16-bit integer.
    ///
    /// @param s The ending bit position (0-15).
    ///
    /// @return A bitmask with bits set from the least significant bit up to position s.
    constexpr BASALT_FORCE_INLINE uint16_t IntervalDown16(const uint16_t s)
    {
        DCHECK_LE(s, 15) << "Shift exceeds bit width";
        return static_cast<uint16_t>(kAllBits16 >> (15 - s));
    }

    /// @brief Creates a bitmask with bits set from the least significant bit up to position s for an 8-bit integer.
    ///
    /// @param s The ending bit position (0-7).
    ///
    /// @return A bitmask with bits set from the least significant bit up to position s.
    constexpr BASALT_FORCE_INLINE uint8_t IntervalDown8(const uint8_t s)
    {
        DCHECK_LE(s, 7) << "Shift exceeds bit width";
        return static_cast<uint8_t>(kAllBits8 >> (7 - s));
    }

    /// @brief Generic template to create a bitmask with bits set from the least significant bit up to position s.
    ///
    /// @tparam T An unsigned integer type.
    ///
    /// @param s The ending bit position.
    ///
    /// @return A bitmask of type T with bits set from the least significant bit up to position s.
    // ReSharper disable once CppNotAllPathsReturnValue
    template <typename T>
        requires std::is_unsigned_v<T>
    constexpr BASALT_FORCE_INLINE T IntervalDown(const T s)
    {
        if constexpr (std::is_same_v<T, uint64_t>)
        {
            return IntervalDown64(s);
        }
        else if constexpr (std::is_same_v<T, uint32_t>)
        {
            return IntervalDown32(s);
        }
        else if constexpr (std::is_same_v<T, uint16_t>)
        {
            return IntervalDown16(s);
        }
        else if constexpr (std::is_same_v<T, uint8_t>)
        {
            return IntervalDown8(s);
        }
        else
        {
            static_assert(sizeof(T) == 0, "Unsupported type for IntervalDown");
        }
    }
}

#endif // BASALT_UTILS_BIT_H_
