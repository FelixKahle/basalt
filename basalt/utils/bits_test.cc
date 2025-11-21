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

#include <random>
#include <limits>
#include <bitset>
#include <type_traits>
#include "gtest/gtest.h"
#include "basalt/utils/bits.h"

namespace bslt::bits::test
{
    // Constants
    static_assert(kAllBits64 == 0xFFFFFFFFFFFFFFFFULL, "kAllBits64 failed");
    static_assert(kAllBits32 == 0xFFFFFFFFU, "kAllBits32 failed");
    static_assert(kAllBits16 == 0xFFFFU, "kAllBits16 failed");
    static_assert(kAllBits8 == 0xFFU, "kAllBits8 failed");

    // BitMask
    static_assert(BitMask64(63) == 0x8000000000000000ULL, "BitMask64 failed");
    static_assert(BitMask32(31) == 0x80000000U, "BitMask32 failed");
    static_assert(BitMask16(15) == 0x8000U, "BitMask16 failed");
    static_assert(BitMask8(7) == 0x80U, "BitMask8 failed");

    // BitMaskRange
    static_assert(BitMaskRange64(0, 63) == 0xFFFFFFFFFFFFFFFFULL, "BitMaskRange64 full failed");
    static_assert(BitMaskRange64(1, 3) == 0x0EULL, "BitMaskRange64 subset failed");
    static_assert(BitMaskRange32(8, 15) == 0x0000FF00U, "BitMaskRange32 middle failed");
    static_assert(BitMaskRange16(0, 0) == 0x0001U, "BitMaskRange16 single bit failed");
    static_assert(BitMaskRange8(4, 7) == 0xF0U, "BitMaskRange8 upper half failed");

    // InverseBitMask
    static_assert(InverseBitMask64(0) == 0xFFFFFFFFFFFFFFFEULL, "InverseBitMask64 failed");
    static_assert(InverseBitMask32(0) == 0xFFFFFFFEU, "InverseBitMask32 failed");
    static_assert(InverseBitMask16(0) == 0xFFFEU, "InverseBitMask16 failed");
    static_assert(InverseBitMask8(0) == 0xFEU, "InverseBitMask8 failed");

    // InverseBitMaskRange
    static_assert(InverseBitMaskRange64(0, 63) == 0ULL, "InverseBitMaskRange64 full failed");
    static_assert(InverseBitMaskRange32(0, 31) == 0U, "InverseBitMaskRange32 full failed");
    static_assert(InverseBitMaskRange16(4, 7) == 0xFF0FU, "InverseBitMaskRange16 middle failed");
    static_assert(InverseBitMaskRange8(0, 3) == 0xF0U, "InverseBitMaskRange8 lower half failed");

    // BitCount
    static_assert(BitCount64(0x0000FFFF0000FFFFULL) == 32, "BitCount64 failed");
    static_assert(BitCount32(0x00FF00FFU) == 16, "BitCount32 failed");
    static_assert(BitCount16(0x0F0FU) == 8, "BitCount16 failed");
    static_assert(BitCount8(0x55U) == 4, "BitCount8 failed");

    // LeastSignificantBitWord
    static_assert(LeastSignificantBitWord64(48ULL) == 16ULL, "LSBWord64 failed");
    static_assert(LeastSignificantBitWord32(48U) == 16U, "LSBWord32 failed");
    static_assert(LeastSignificantBitWord16(48U) == 16U, "LSBWord16 failed");
    static_assert(LeastSignificantBitWord8(48U) == 16U, "LSBWord8 failed");

    // LeastSignificantBitPosition
    static_assert(LeastSignificantBitPosition64(1ULL << 63) == 63, "LSBPos64 failed");
    static_assert(LeastSignificantBitPosition32(1U << 31) == 31, "LSBPos32 failed");
    static_assert(LeastSignificantBitPosition16(1U << 15) == 15, "LSBPos16 failed");
    static_assert(LeastSignificantBitPosition8(1U << 7) == 7, "LSBPos8 failed");

    // MostSignificantBitWord
    static_assert(MostSignificantBitWord64(0xFFFF000000000000ULL) == 0x8000000000000000ULL, "MSBWord64 failed");
    static_assert(MostSignificantBitWord32(0x00F00000U) == 0x00800000U, "MSBWord32 failed");
    static_assert(MostSignificantBitWord16(0x0030U) == 0x0020U, "MSBWord16 failed");
    static_assert(MostSignificantBitWord8(0x03U) == 0x02U, "MSBWord8 failed");

    // MostSignificantBitPosition
    static_assert(MostSignificantBitPosition64(0x8000000000000000ULL) == 63, "MSBPos64 failed");
    static_assert(MostSignificantBitPosition64(3ULL) == 1, "MSBPos64 small value failed");
    static_assert(MostSignificantBitPosition32(0x80000000U) == 31, "MSBPos32 failed");
    static_assert(MostSignificantBitPosition16(0x00FFU) == 7, "MSBPos16 failed");
    static_assert(MostSignificantBitPosition8(0x01U) == 0, "MSBPos8 failed");

    // BitPosition (Index within word)
    static_assert(BitPosition64(65) == 1, "BitPosition64 failed");
    static_assert(BitPosition32(33) == 1, "BitPosition32 failed");
    static_assert(BitPosition16(17) == 1, "BitPosition16 failed");
    static_assert(BitPosition8(9) == 1, "BitPosition8 failed");

    // BitOffset (Array index)
    static_assert(BitOffset64(128) == 2, "BitOffset64 failed");
    static_assert(BitOffset32(64) == 2, "BitOffset32 failed");
    static_assert(BitOffset16(32) == 2, "BitOffset16 failed");
    static_assert(BitOffset8(16) == 2, "BitOffset8 failed");

    // BitLength (Storage size required)
    static_assert(BitLength64(129) == 3, "BitLength64 failed");
    static_assert(BitLength32(65) == 3, "BitLength32 failed");
    static_assert(BitLength16(33) == 3, "BitLength16 failed");
    static_assert(BitLength8(17) == 3, "BitLength8 failed");

    // IntervalUp (Masks from S up to MSB)
    static_assert(IntervalUp64(0) == kAllBits64, "IntervalUp64 full failed");
    static_assert(IntervalUp64(63) == 0x8000000000000000ULL, "IntervalUp64 msb failed");
    static_assert(IntervalUp32(16) == 0xFFFF0000U, "IntervalUp32 half failed");
    static_assert(IntervalUp8(4) == 0xF0U, "IntervalUp8 failed");

    // IntervalDown (Masks from 0 up to S)
    static_assert(IntervalDown64(63) == kAllBits64, "IntervalDown64 full failed");
    static_assert(IntervalDown64(0) == 1ULL, "IntervalDown64 lsb failed");
    static_assert(IntervalDown32(15) == 0x0000FFFFU, "IntervalDown32 half failed");
    static_assert(IntervalDown8(3) == 0x0FU, "IntervalDown8 failed");

    template <typename T>
    class BitUtilsTypedTest : public ::testing::Test
    {
    };

    using UnsignedTypes = ::testing::Types<uint8_t, uint16_t, uint32_t, uint64_t>;
    TYPED_TEST_SUITE(BitUtilsTypedTest, UnsignedTypes);

    TYPED_TEST(BitUtilsTypedTest, ConstantsCorrectness)
    {
        using T = TypeParam;

        // We can't test kAllBits<T> generically easily without SFINAE mapping,
        // but we can verify the logic via masks.
        EXPECT_EQ(BitMask<T>(0), T{1});
    }

    TYPED_TEST(BitUtilsTypedTest, BitMaskAndInverse)
    {
        using T = TypeParam;
        constexpr uint32_t kBits = sizeof(T) * 8;

        for (uint32_t i = 0; i < kBits; ++i)
        {
            const T mask = BitMask<T>(i);
            const T inverse = InverseBitMask<T>(i);
            const T expectedMask = static_cast<T>(T{1} << i);

            EXPECT_EQ(mask, expectedMask) << "BitMask failed at index " << i;
            EXPECT_EQ(inverse, static_cast<T>(~mask)) << "InverseBitMask failed at index " << i;

            // Property: Mask | Inverse should be all 1s (Max)
            // Note: uint8/16 promotion to int requires cast back
            EXPECT_EQ(static_cast<T>(mask | inverse), std::numeric_limits<T>::max());

            // Property: Mask & Inverse should be 0
            EXPECT_EQ(static_cast<T>(mask & inverse), T{0});
        }
    }

    TYPED_TEST(BitUtilsTypedTest, BitCountBasics)
    {
        using T = TypeParam;
        EXPECT_EQ(BitCount<T>(0), 0);
        EXPECT_EQ(BitCount<T>(std::numeric_limits<T>::max()), sizeof(T) * 8);

        // Test pattern 0x55 (0101...)
        T pattern55 = 0;
        for (size_t i = 0; i < sizeof(T); ++i) pattern55 |= static_cast<T>(0x55ULL << (i * 8));
        EXPECT_EQ(BitCount<T>(pattern55), (sizeof(T) * 8) / 2);
    }

    TYPED_TEST(BitUtilsTypedTest, LeastSignificantBitWord)
    {
        using T = TypeParam;
        constexpr uint32_t kBits = sizeof(T) * 8;

        EXPECT_EQ(LeastSignificantBitWord<T>(0), 0);

        for (uint32_t i = 0; i < kBits; ++i)
        {
            T val = BitMask<T>(i);
            // 1. Pure power of 2: LSB Word is the value itself
            EXPECT_EQ(LeastSignificantBitWord<T>(val), val);

            // 2. Add noise bits ABOVE the LSB
            if (i < kBits - 1)
            {
                // Set the highest bit as noise
                T noise = static_cast<T>(val | BitMask<T>(kBits - 1));
                EXPECT_EQ(LeastSignificantBitWord<T>(noise), val);
            }
        }
    }

    TYPED_TEST(BitUtilsTypedTest, LeastSignificantBitPosition)
    {
        using T = TypeParam;
        constexpr uint32_t kBits = sizeof(T) * 8;

        // Undefined for 0, so start loops at finding set bits
        for (uint32_t i = 0; i < kBits; ++i)
        {
            T val = BitMask<T>(i);
            EXPECT_EQ(LeastSignificantBitPosition<T>(val), i);

            // Test with noise above
            if (i < kBits - 1)
            {
                T noise = static_cast<T>(val | BitMask<T>(kBits - 1));
                EXPECT_EQ(LeastSignificantBitPosition<T>(noise), i);
            }
        }
    }

    TYPED_TEST(BitUtilsTypedTest, MostSignificantBitPosition)
    {
        using T = TypeParam;
        constexpr uint32_t kBits = sizeof(T) * 8;

        for (uint32_t i = 0; i < kBits; ++i)
        {
            T val = BitMask<T>(i);

            // Case 1: Exact power of 2
            EXPECT_EQ(MostSignificantBitPosition<T>(val), i);

            // Case 2: Add noise BELOW the MSB
            if (i > 0)
            {
                T noise = static_cast<T>(val | (val - 1));
                EXPECT_EQ(MostSignificantBitPosition<T>(noise), i)
                    << "Failed with lower-bit noise at index " << i;
            }
        }
    }

    TYPED_TEST(BitUtilsTypedTest, MostSignificantBitWord)
    {
        using T = TypeParam;
        constexpr uint32_t kBits = sizeof(T) * 8;

        for (uint32_t i = 0; i < kBits; ++i)
        {
            T expectedMsb = BitMask<T>(i); // The single bit at position i

            // Case 1: Exact power of 2
            // The MSB Word of a power of 2 is the value itself.
            EXPECT_EQ(MostSignificantBitWord<T>(expectedMsb), expectedMsb);

            // Case 2: Add noise BELOW the MSB
            // e.g., if MSB is at 4 (10000), adding 01111 (15) -> 11111.
            // The MSB Word should still be 10000.
            if (i > 0)
            {
                T noise = static_cast<T>(expectedMsb | (expectedMsb - 1));
                EXPECT_EQ(MostSignificantBitWord<T>(noise), expectedMsb)
                    << "Failed with lower-bit noise at index " << i;
            }
        }
    }

    TYPED_TEST(BitUtilsTypedTest, BitPosition)
    {
        using T = TypeParam;
        constexpr uint32_t kBits = sizeof(T) * 8;

        // BitPosition<T> is (pos % kBits)
        EXPECT_EQ(BitPosition<T>(0), 0);
        EXPECT_EQ(BitPosition<T>(kBits - 1), kBits - 1);
        EXPECT_EQ(BitPosition<T>(kBits), 0); // Wrap
        EXPECT_EQ(BitPosition<T>(kBits + 1), 1); // Wrap
    }

    TYPED_TEST(BitUtilsTypedTest, BitOffset)
    {
        using T = TypeParam;
        constexpr uint32_t kBits = sizeof(T) * 8;

        // BitOffset<T> is (pos / kBits)
        EXPECT_EQ(BitOffset<T>(0), 0);
        EXPECT_EQ(BitOffset<T>(kBits - 1), 0);
        EXPECT_EQ(BitOffset<T>(kBits), 1);
        EXPECT_EQ(BitOffset<T>(kBits * 10 + 5), 10);
    }

    TYPED_TEST(BitUtilsTypedTest, BitLength)
    {
        using T = TypeParam;
        constexpr uint32_t kBits = sizeof(T) * 8;

        // How many elements of type T are needed to store N bits?
        EXPECT_EQ(BitLength<T>(0), 0);
        EXPECT_EQ(BitLength<T>(1), 1);
        EXPECT_EQ(BitLength<T>(kBits), 1);
        EXPECT_EQ(BitLength<T>(kBits + 1), 2);
        EXPECT_EQ(BitLength<T>(kBits * 2), 2);
    }

    TYPED_TEST(BitUtilsTypedTest, BitMaskRangeCorrectness)
    {
        using T = TypeParam;
        constexpr uint32_t kBits = sizeof(T) * 8;

        // Test every possible valid range
        for (uint32_t start = 0; start < kBits; ++start)
        {
            for (uint32_t end = start; end < kBits; ++end)
            {
                const T mask = BitMaskRange<T>(start, end);
                const T inverse = InverseBitMaskRange<T>(start, end);

                // 1. Verify Inverse is complement
                EXPECT_EQ(mask, static_cast<T>(~inverse));

                // 2. Verify manually
                T expected = 0;
                for (uint32_t i = start; i <= end; ++i)
                {
                    expected |= static_cast<T>(T{1} << i);
                }
                EXPECT_EQ(mask, expected) << "Range [" << start << ", " << end << "]";
            }
        }
    }

    TYPED_TEST(BitUtilsTypedTest, IntervalCorrectness)
    {
        using T = TypeParam;
        constexpr uint32_t kBits = sizeof(T) * 8;

        for (uint32_t i = 0; i < kBits; ++i)
        {
            // IntervalUp(i) should equivalent to Range(i, max_bit)
            // It sets bits [i, kBits-1]
            T up = IntervalUp<T>(static_cast<T>(i));
            T upRange = BitMaskRange<T>(i, kBits - 1);
            EXPECT_EQ(up, upRange) << "IntervalUp(" << i << ")";

            // IntervalDown(i) should be equivalent to Range(0, i)
            // It sets bits [0, i]
            T down = IntervalDown<T>(static_cast<T>(i));
            T downRange = BitMaskRange<T>(0, i);
            EXPECT_EQ(down, downRange) << "IntervalDown(" << i << ")";
        }
    }

    TEST(ExplicitApiTest, Constants)
    {
        EXPECT_EQ(kAllBits64, std::numeric_limits<uint64_t>::max());
        EXPECT_EQ(kAllBits32, std::numeric_limits<uint32_t>::max());
        EXPECT_EQ(kAllBits16, std::numeric_limits<uint16_t>::max());
        EXPECT_EQ(kAllBits8, std::numeric_limits<uint8_t>::max());

        EXPECT_EQ(kAllBitsButLsb64, 0xFFFFFFFFFFFFFFFEULL);
        EXPECT_EQ(kAllBitsButLsb8, 0xFE);
    }

    TEST(ExplicitApiTest, MaskGenerators)
    {
        EXPECT_EQ(BitMask64(1), 2ULL);
        EXPECT_EQ(BitMask32(1), 2U);
        EXPECT_EQ(BitMask16(1), 2U);
        EXPECT_EQ(BitMask8(1), 2U);

        EXPECT_EQ(InverseBitMask64(0), 0xFFFFFFFFFFFFFFFEULL);
        EXPECT_EQ(InverseBitMask32(0), 0xFFFFFFFEU);
        EXPECT_EQ(InverseBitMask16(0), 0xFFFEU);
        EXPECT_EQ(InverseBitMask8(0), 0xFEU);
    }

    TEST(ExplicitApiTest, PopCount)
    {
        EXPECT_EQ(BitCount64(0x3), 2);
        EXPECT_EQ(BitCount32(0x3), 2);
        EXPECT_EQ(BitCount16(0x3), 2);
        EXPECT_EQ(BitCount8(0x3), 2);
    }

    TEST(ExplicitApiTest, LSBWord)
    {
        EXPECT_EQ(LeastSignificantBitWord64(12), 4);
        EXPECT_EQ(LeastSignificantBitWord32(12), 4);
        EXPECT_EQ(LeastSignificantBitWord16(12), 4);
        EXPECT_EQ(LeastSignificantBitWord8(12), 4);
    }

    TEST(ExplicitApiTest, LSBPosition)
    {
        // 12 = 1100 binary, LSB is at index 2
        EXPECT_EQ(LeastSignificantBitPosition64(12), 2);
        EXPECT_EQ(LeastSignificantBitPosition32(12), 2);
        EXPECT_EQ(LeastSignificantBitPosition16(12), 2);
        EXPECT_EQ(LeastSignificantBitPosition8(12), 2);
    }

    TEST(ExplicitApiTest, MSBPosition)
    {
        // 12 = 1100 binary, MSB is at index 3
        EXPECT_EQ(MostSignificantBitPosition64(12), 3);
        EXPECT_EQ(MostSignificantBitPosition32(12), 3);
        EXPECT_EQ(MostSignificantBitPosition16(12), 3);
        EXPECT_EQ(MostSignificantBitPosition8(12), 3);

        // 1 = 0001 binary, MSB is at index 0
        EXPECT_EQ(MostSignificantBitPosition32(1), 0);
    }

    TEST(ExplicitApiTest, MSBWord)
    {
        // 0b00001100 (12) -> MSB is 0b00001000 (8)
        EXPECT_EQ(MostSignificantBitWord64(12), 8);
        EXPECT_EQ(MostSignificantBitWord32(12), 8);
        EXPECT_EQ(MostSignificantBitWord16(12), 8);
        EXPECT_EQ(MostSignificantBitWord8(12), 8);

        // Edge Case: Max value
        // 0xFF (255) -> MSB is 0x80 (128)
        EXPECT_EQ(MostSignificantBitWord8(255), 128);

        // Edge Case: Smallest non-zero
        EXPECT_EQ(MostSignificantBitWord64(1), 1);
    }

    TEST(ExplicitApiTest, DeBruijnFallback)
    {
        // Explicitly test the DeBruijn fallback function for 64-bit
        // to ensure the math works even if the compiler picks intrinsics for the main function.
        for (int i = 0; i < 64; ++i)
        {
            EXPECT_EQ(LeastSignificantBitPosition64DeBruijn(1ULL << i), i);
        }
    }

    TEST(ExplicitApiTest, StorageUtils)
    {
        // 64-bit storage helpers
        EXPECT_EQ(BitPosition64(66), 2);
        EXPECT_EQ(BitOffset64(66), 1);
        EXPECT_EQ(BitLength64(66), 2);

        // 8-bit storage helpers
        EXPECT_EQ(BitPosition8(9), 1);
        EXPECT_EQ(BitOffset8(9), 1);
        EXPECT_EQ(BitLength8(9), 2);
    }

    TEST(ExplicitApiTest, RangeMasks)
    {
        // 64-bit
        EXPECT_EQ(BitMaskRange64(0, 1), 3ULL);
        EXPECT_EQ(BitMaskRange64(62, 63), 0xC000000000000000ULL);

        // 32-bit
        EXPECT_EQ(BitMaskRange32(0, 0), 1U);
        EXPECT_EQ(BitMaskRange32(0, 31), 0xFFFFFFFFU);

        // 16-bit
        EXPECT_EQ(BitMaskRange16(4, 7), 0x00F0U);

        // 8-bit
        EXPECT_EQ(BitMaskRange8(1, 6), 0x7EU); // 01111110
    }

    TEST(ExplicitApiTest, InverseRangeMasks)
    {
        // Inverse of Range(0, 1) -> Inverse of ...00011 -> ...11100
        EXPECT_EQ(InverseBitMaskRange64(0, 1), 0xFFFFFFFFFFFFFFFCULL);

        // Inverse of Range(1, 6) 8-bit -> ~0x7E -> 0x81
        EXPECT_EQ(InverseBitMaskRange8(1, 6), 0x81U);
    }

    TEST(ExplicitApiTest, Intervals)
    {
        // Up: [s, 63]
        EXPECT_EQ(IntervalUp64(63), 0x8000000000000000ULL);
        EXPECT_EQ(IntervalUp8(4), 0xF0U); // 11110000

        // Down: [0, s]
        EXPECT_EQ(IntervalDown64(0), 1ULL);
        EXPECT_EQ(IntervalDown8(3), 0x0FU); // 00001111
    }

#ifndef NDEBUG
    TEST(BitUtilsDeathTest, LSBZeroInput)
    {
        // LSB position is mathematically undefined for 0.
        // The library uses DCHECK_NE.
        EXPECT_DEATH(LeastSignificantBitPosition64(0), "LSB position is undefined");
        EXPECT_DEATH(LeastSignificantBitPosition32(0), "LSB position is undefined");
        EXPECT_DEATH(LeastSignificantBitPosition16(0), "LSB position is undefined");
        EXPECT_DEATH(LeastSignificantBitPosition8(0), "LSB position is undefined");
        EXPECT_DEATH(LeastSignificantBitPosition64DeBruijn(0), "LSB position is undefined");
    }

    TEST(BitUtilsDeathTest, MSBZeroInput)
    {
        // MSB is mathematically undefined for 0.
        // The library uses DCHECK_NE.
        EXPECT_DEATH(MostSignificantBitWord64(0), "MSB word is undefined");
        EXPECT_DEATH(MostSignificantBitWord32(0), "MSB word is undefined");
        EXPECT_DEATH(MostSignificantBitWord16(0), "MSB word is undefined");
        EXPECT_DEATH(MostSignificantBitWord8(0), "MSB word is undefined");

        EXPECT_DEATH(MostSignificantBitPosition64(0), "MSB position is undefined");
        EXPECT_DEATH(MostSignificantBitPosition32(0), "MSB position is undefined");
        EXPECT_DEATH(MostSignificantBitPosition16(0), "MSB position is undefined");
        EXPECT_DEATH(MostSignificantBitPosition8(0), "MSB position is undefined");
    }

    TEST(BitUtilsDeathTest, MaskOutOfBounds)
    {
        // Shifting >= width is UB in C++. The library uses DCHECK_LT.
        EXPECT_DEATH(BitMask64(64), "Shift amount must be less than");
        EXPECT_DEATH(BitMask32(32), "Shift amount must be less than");
        EXPECT_DEATH(InverseBitMask16(16), "Shift amount must be less than");
    }

    TEST(BitUtilsDeathTest, RangeInvalid)
    {
        // Start > End
        EXPECT_DEATH(BitMaskRange64(10, 5), "Start position must be less than or equal to end");
        EXPECT_DEATH(BitMaskRange32(5, 4), "Start position must be less than or equal to end");

        // Out of bounds
        EXPECT_DEATH(BitMaskRange8(0, 8), "End position must be less than 8");
        EXPECT_DEATH(BitMaskRange8(8, 9), "Start position must be less than 8");
    }

    TEST(BitUtilsDeathTest, InverseRangeInvalid)
    {
        EXPECT_DEATH(InverseBitMaskRange64(10, 5), "Start position must be less than or equal to end");
        EXPECT_DEATH(InverseBitMaskRange8(0, 8), "End position must be less than 8");
    }

    TEST(BitUtilsDeathTest, IntervalInvalid)
    {
        // Shift exceeds bit width (Interval functions take shift amount s, must be < width)
        EXPECT_DEATH(IntervalUp64(64), "Shift exceeds bit width"); // DCHECK_LE(s, 63)
        EXPECT_DEATH(IntervalDown8(8), "Shift exceeds bit width"); // DCHECK_LE(s, 7)
    }
#endif

    // Helper to calculate LSB naively to verify optimized implementations
    template <typename T>
    static int NaiveLSB(T n)
    {
        if (n == 0) return 0; // Should not happen in test generation
        for (int i = 0; i < static_cast<int>(sizeof(T) * 8); ++i)
        {
            if ((n >> i) & 1) return i;
        }
        return -1;
    }

    template <typename T>
    static int NaiveMSB(T n)
    {
        if (n == 0) return 0;
        int msbIndex = 0;
        while (n >>= 1)
        {
            msbIndex++;
        }
        return msbIndex;
    }

    TEST(BitUtilsFuzzTest, PopCount64AgainstBitset)
    {
        std::mt19937_64 rng(42); // NOLINT(*-msc51-cpp)
        std::uniform_int_distribution<uint64_t> dist;

        for (int i = 0; i < 1000; ++i)
        {
            uint64_t val = dist(rng);
            EXPECT_EQ(BitCount64(val), std::bitset<64>(val).count());
        }
    }

    TEST(BitUtilsFuzzTest, PopCount32AgainstBitset)
    {
        std::mt19937 rng(42); // NOLINT(*-msc51-cpp)
        std::uniform_int_distribution<uint32_t> dist;

        for (int i = 0; i < 1000; ++i)
        {
            uint32_t val = dist(rng);
            EXPECT_EQ(BitCount32(val), std::bitset<32>(val).count());
        }
    }

    TEST(BitUtilsFuzzTest, LSBPos64AgainstNaive)
    {
        std::mt19937_64 rng(123); // NOLINT(*-msc51-cpp)
        // Range starts at 1 because LSB(0) is undefined
        std::uniform_int_distribution<uint64_t> dist(1, std::numeric_limits<uint64_t>::max());

        for (int i = 0; i < 1000; ++i)
        {
            uint64_t val = dist(rng);
            EXPECT_EQ(LeastSignificantBitPosition64(val), NaiveLSB(val));
        }
    }

    TEST(BitUtilsFuzzTest, LSBPos32AgainstNaive)
    {
        std::mt19937 rng(123); // NOLINT(*-msc51-cpp)
        std::uniform_int_distribution<uint32_t> dist(1, std::numeric_limits<uint32_t>::max());

        for (int i = 0; i < 1000; ++i)
        {
            uint32_t val = dist(rng);
            EXPECT_EQ(LeastSignificantBitPosition32(val), NaiveLSB(val));
        }
    }

    TEST(BitUtilsFuzzTest, MSBPos64AgainstNaive)
    {
        std::mt19937_64 rng(999); // NOLINT(*-msc51-cpp)
        // Range starts at 1 because MSB(0) is undefined
        std::uniform_int_distribution<uint64_t> dist(1, std::numeric_limits<uint64_t>::max());

        for (int i = 0; i < 1000; ++i)
        {
            uint64_t val = dist(rng);
            EXPECT_EQ(MostSignificantBitPosition64(val), NaiveMSB(val))
                << "Failed for value: " << val;
        }
    }

    TEST(BitUtilsFuzzTest, MSBPos32AgainstNaive)
    {
        std::mt19937 rng(999); // NOLINT(*-msc51-cpp)
        std::uniform_int_distribution<uint32_t> dist(1, std::numeric_limits<uint32_t>::max());

        for (int i = 0; i < 1000; ++i)
        {
            uint32_t val = dist(rng);
            EXPECT_EQ(MostSignificantBitPosition32(val), NaiveMSB(val))
                << "Failed for value: " << val;
        }
    }

    TEST(BitUtilsFuzzTest, MSBWord64Correctness)
    {
        std::mt19937_64 rng(888); // NOLINT(*-msc51-cpp)
        std::uniform_int_distribution<uint64_t> dist(1, std::numeric_limits<uint64_t>::max());

        for (int i = 0; i < 1000; ++i)
        {
            uint64_t val = dist(rng);
            uint64_t msbWord = MostSignificantBitWord64(val);

            // Check 1: The result is a power of 2 (only 1 bit set)
            EXPECT_EQ(BitCount64(msbWord), 1);

            // Check 2: The result is <= original value
            EXPECT_LE(msbWord, val);

            // Check 3: Doubling the result (if no overflow) should be > original value
            if (msbWord != (1ULL << 63))
            {
                EXPECT_GT(msbWord << 1, val);
            }
        }
    }
}
