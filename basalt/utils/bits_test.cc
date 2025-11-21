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
#include <vector>
#include <bitset>
#include <type_traits>
#include <algorithm>
#include "gtest/gtest.h"
#include "basalt/utils/bits.h"

namespace bslt::bits::test
{
    static_assert(kAllBits64 == 0xFFFFFFFFFFFFFFFFULL, "kAllBits64 constexpr failed");
    static_assert(kAllBits32 == 0xFFFFFFFFU, "kAllBits32 constexpr failed");
    static_assert(kAllBits16 == 0xFFFFU, "kAllBits16 constexpr failed");
    static_assert(kAllBits8 == 0xFFU, "kAllBits8 constexpr failed");

    static_assert(BitMask64(63) == 0x8000000000000000ULL, "BitMask64 constexpr failed");
    static_assert(BitMask32(31) == 0x80000000U, "BitMask32 constexpr failed");
    static_assert(BitMask16(15) == 0x8000U, "BitMask16 constexpr failed");
    static_assert(BitMask8(7) == 0x80U, "BitMask8 constexpr failed");

    static_assert(BitMaskRange64(0, 63) == kAllBits64, "BitMaskRange64 full constexpr failed");
    static_assert(BitMaskRange64(1, 2) == 0x06ULL, "BitMaskRange64 sub constexpr failed");
    static_assert(BitMaskRange32(0, 31) == kAllBits32, "BitMaskRange32 full constexpr failed");
    static_assert(BitMaskRange8(0, 7) == kAllBits8, "BitMaskRange8 full constexpr failed");
    static_assert(BitMaskRange64(63, 63) == 0x8000000000000000ULL, "BitMaskRange64 single-bit high failed");
    static_assert(BitMaskRange64(0, 0) == 0x1ULL, "BitMaskRange64 single-bit low failed");

    static_assert(InverseBitMask64(0) == ~1ULL, "InverseBitMask64 constexpr failed");
    static_assert(InverseBitMask32(0) == ~1U, "InverseBitMask32 constexpr failed");

    static_assert(InverseBitMaskRange64(0, 63) == 0ULL, "InverseBitMaskRange64 full constexpr failed");
    static_assert(InverseBitMaskRange8(1, 6) == 0x81U, "InverseBitMaskRange8 partial constexpr failed");

    static_assert(BitCount64(0xFFFFFFFFFFFFFFFFULL) == 64, "BitCount64 constexpr failed");
    static_assert(BitCount32(0x0000FFFFU) == 16, "BitCount32 constexpr failed");
    static_assert(BitCount16(0x0F0FU) == 8, "BitCount16 constexpr failed");
    static_assert(BitCount8(0x03U) == 2, "BitCount8 constexpr failed");

    static_assert(LeastSignificantBitWord64(0x1010ULL) == 0x0010ULL, "LSBWord64 constexpr failed");
    static_assert(MostSignificantBitWord64(0x1010ULL) == 0x1000ULL, "MSBWord64 constexpr failed");

    static_assert(LeastSignificantBitPosition64(0x8000000000000000ULL) == 63, "LSBPos64 constexpr failed");
    static_assert(MostSignificantBitPosition64(0x0000000000000001ULL) == 0, "MSBPos64 constexpr failed");

    static_assert(IntervalUp64(63) == 0x8000000000000000ULL, "IntervalUp64 constexpr failed");
    static_assert(IntervalDown64(0) == 1ULL, "IntervalDown64 constexpr failed");

    static_assert(BitLength64(65) == 2, "BitLength64 constexpr failed");
    static_assert(BitOffset64(64) == 1, "BitOffset64 constexpr failed");
    static_assert(BitPosition64(65) == 1, "BitPosition64 constexpr failed");

    template <typename T>
    class BitUtilsTypedTest : public ::testing::Test
    {
    };

    using UnsignedTypes = ::testing::Types<uint8_t, uint16_t, uint32_t, uint64_t>;
    TYPED_TEST_SUITE(BitUtilsTypedTest, UnsignedTypes);

    TYPED_TEST(BitUtilsTypedTest, BitMaskAndInverseConsistency)
    {
        using T = TypeParam;
        constexpr uint32_t k_bits = sizeof(T) * 8;

        for (uint32_t i = 0; i < k_bits; ++i)
        {
            const T mask = BitMask<T>(i);
            const T inverse = InverseBitMask<T>(i);
            const T expected = static_cast<T>(T{1} << i);

            EXPECT_EQ(mask, expected);
            EXPECT_EQ(inverse, static_cast<T>(~mask));
            // Mask + Inverse should equal all 1s (implicitly max value)
            EXPECT_EQ(static_cast<T>(mask | inverse), std::numeric_limits<T>::max());
            EXPECT_EQ(static_cast<T>(mask & inverse), T{0});
        }
    }

    TYPED_TEST(BitUtilsTypedTest, BitMaskRangeConsistency)
    {
        using T = TypeParam;
        constexpr uint32_t k_bits = sizeof(T) * 8;

        // Test subset of ranges to save time, but cover boundaries
        std::vector<std::pair<uint32_t, uint32_t>> ranges = {
            {0, 0}, {0, k_bits - 1}, {k_bits / 2, k_bits - 1}, {0, k_bits / 2},
            {k_bits - 1, k_bits - 1} // Single bit at MSB
        };

        for (auto [start, end] : ranges)
        {
            const T mask = BitMaskRange<T>(start, end);
            const T inverse = InverseBitMaskRange<T>(start, end);

            EXPECT_EQ(mask, static_cast<T>(~inverse));

            // Verify manually
            T calculated = 0;
            for (uint32_t i = start; i <= end; ++i)
            {
                calculated |= static_cast<T>(T{1} << i);
            }
            EXPECT_EQ(mask, calculated);
        }
    }

    TYPED_TEST(BitUtilsTypedTest, PopcountPatterns)
    {
        using T = TypeParam;
        constexpr uint32_t k_bits = sizeof(T) * 8;

        // Alternating bits 0xAA...
        T val_aa = 0;
        for (uint32_t i = 1; i < k_bits; i += 2) val_aa |= static_cast<T>(T{1} << i);
        EXPECT_EQ(BitCount<T>(val_aa), k_bits / 2);

        // Alternating bits 0x55...
        T val_55 = 0;
        for (uint32_t i = 0; i < k_bits; i += 2) val_55 |= static_cast<T>(T{1} << i);
        EXPECT_EQ(BitCount<T>(val_55), k_bits / 2);

        // All bits
        EXPECT_EQ(BitCount<T>(std::numeric_limits<T>::max()), k_bits);

        // No bits
        EXPECT_EQ(BitCount<T>(T{0}), 0);
    }

    TYPED_TEST(BitUtilsTypedTest, WordIsolationComplex)
    {
        using T = TypeParam;
        // 1100...0011
        T val = static_cast<T>(BitMask<T>(0) | BitMask<T>(1));
        if (sizeof(T) * 8 > 2)
        {
            val |= static_cast<T>(MostSignificantBitWord<T>(std::numeric_limits<T>::max()));
        }

        // LSB should be bit 0 (value 1)
        EXPECT_EQ(LeastSignificantBitWord<T>(val), T{1});

        // MSB should be the top bit
        EXPECT_EQ(MostSignificantBitWord<T>(val), MostSignificantBitWord<T>(std::numeric_limits<T>::max()));
    }

    TYPED_TEST(BitUtilsTypedTest, ArrayManipulationAndQuery)
    {
        using T = TypeParam;
        constexpr uint64_t k_bits_per_elem = sizeof(T) * 8;
        constexpr size_t k_arr_size = 4;

        T data[k_arr_size] = {0};

        SetBit<T>(data, 0); // Word 0, Bit 0
        EXPECT_EQ(data[0], T{1});
        EXPECT_TRUE(IsBitSet<T>(data, 0));

        const uint64_t word1_bit = k_bits_per_elem;
        SetBit<T>(data, word1_bit); // Word 1, Bit 0
        EXPECT_EQ(data[1], T{1});
        EXPECT_TRUE(IsBitSet<T>(data, word1_bit));

        ClearBit<T>(data, 0);
        EXPECT_EQ(data[0], T{0});
        EXPECT_FALSE(IsBitSet<T>(data, 0));

        // Fill Word 2 entirely
        const uint64_t start_w2 = 2 * k_bits_per_elem;
        for (uint64_t i = 0; i < k_bits_per_elem; ++i)
        {
            SetBit<T>(data, start_w2 + i);
        }

        // Count all set bits in array (Should be Word 1 bit (1) + Word 2 bits (all))
        EXPECT_EQ(BitCountRange<T>(data, 0, k_arr_size * k_bits_per_elem - 1), 1 + k_bits_per_elem);

        // Count partial
        EXPECT_EQ(BitCountRange<T>(data, start_w2, start_w2 + 1), 2);

        EXPECT_TRUE(IsEmptyRange<T>(data, 0, k_bits_per_elem - 1)); // Word 0 is empty
        EXPECT_FALSE(IsEmptyRange<T>(data, start_w2, start_w2 + 5)); // Word 2 is full
    }

    TYPED_TEST(BitUtilsTypedTest, RangeSearchGeneric)
    {
        using T = TypeParam;
        constexpr uint64_t k_bits_per_elem = sizeof(T) * 8;
        constexpr size_t k_arr_size = 4;
        const uint64_t total_bits = k_arr_size * k_bits_per_elem;

        // [0]: 0, [1]: 0, [2]: 1 (LSB), [3]: 0
        T data[k_arr_size] = {0};
        const uint64_t target_bit = 2 * k_bits_per_elem;
        SetBit<T>(data, target_bit);

        // Clamp search end to total_bits - 1
        const uint64_t search_end = std::min(target_bit + 10, total_bits - 1);
        EXPECT_EQ(LeastSignificantBitPosition<T>(data, 0, search_end), static_cast<int64_t>(target_bit));
        EXPECT_EQ(MostSignificantBitPosition<T>(data, 0, search_end), static_cast<int64_t>(target_bit));
        if (target_bit > 0)
        {
            EXPECT_EQ(LeastSignificantBitPosition<T>(data, 0, target_bit - 1), -1);
        }

        const uint64_t after_start = target_bit + 1;
        const uint64_t after_end = std::min(target_bit + 20, total_bits - 1);
        if (after_start < total_bits)
        {
            EXPECT_EQ(MostSignificantBitPosition<T>(data, after_start, after_end), -1);
        }

        if (const uint64_t high_bit = target_bit + 5; high_bit < total_bits)
        {
            SetBit<T>(data, high_bit);

            // Calculate valid max index (Total bits - 1)
            const uint64_t max_search_index = total_bits - 1;

            // LSB should still be low bit
            EXPECT_EQ(LeastSignificantBitPosition<T>(data, 0, max_search_index), static_cast<int64_t>(target_bit));
            // MSB should be high bit
            EXPECT_EQ(MostSignificantBitPosition<T>(data, 0, max_search_index), static_cast<int64_t>(high_bit));
        }
    }

    TYPED_TEST(BitUtilsTypedTest, RangeSearchSparse)
    {
        // Test finding a bit in a sea of zeros
        using T = TypeParam;
        constexpr uint64_t k_bits_per_elem = sizeof(T) * 8;
        constexpr size_t k_arr_size = 8; // larger array

        T data[k_arr_size] = {0};
        // Set bit in the very last word
        const uint64_t last_bit = (k_arr_size * k_bits_per_elem) - 1;
        SetBit<T>(data, last_bit);

        // Search entire range
        EXPECT_EQ(LeastSignificantBitPosition<T>(data, 0, last_bit), static_cast<int64_t>(last_bit));
        EXPECT_EQ(MostSignificantBitPosition<T>(data, 0, last_bit), static_cast<int64_t>(last_bit));

        // Search from middle to end
        const uint64_t mid_bit = (k_arr_size * k_bits_per_elem) / 2;
        EXPECT_EQ(LeastSignificantBitPosition<T>(data, mid_bit, last_bit), static_cast<int64_t>(last_bit));
    }

    TEST(ExplicitApiTest, MaskGeneratorsCorrectness)
    {
        EXPECT_EQ(BitMask64(1), 2ULL);
        EXPECT_EQ(BitMask32(1), 2U);
        EXPECT_EQ(BitMask16(1), 2U);
        EXPECT_EQ(BitMask8(1), 2U);

        EXPECT_EQ(BitMaskRange64(0, 1), 3ULL);
        EXPECT_EQ(BitMaskRange8(0, 7), 0xFFU);
    }

    TEST(ExplicitApiTest, ArrayOps8Bit)
    {
        // 0xAA = 10101010, 0x55 = 01010101
        constexpr uint8_t arr[] = {0xAA, 0x55};

        // IsBitSet
        EXPECT_FALSE(IsBitSet8(arr, 0));
        EXPECT_TRUE(IsBitSet8(arr, 1));
        EXPECT_TRUE(IsBitSet8(arr, 8)); // Word 1, Bit 0

        // CountRange [0, 15] -> 4 + 4 = 8
        EXPECT_EQ(BitCountRange8(arr, 0, 15), 8);

        // EmptyRange
        EXPECT_FALSE(IsEmptyRange8(arr, 0, 7));
    }

    TEST(ExplicitApiTest, ArrayOps64Bit)
    {
        uint64_t arr[] = {0, 0};
        SetBit64(arr, 127); // Last bit of second word

        EXPECT_TRUE(IsBitSet64(arr, 127));
        EXPECT_EQ(BitCountRange64(arr, 0, 127), 1);
        EXPECT_EQ(MostSignificantBitPosition64(arr, 0, 127), 127);
    }

    TEST(EdgeCaseTest, IntervalBoundaries)
    {
        // Interval functions shift by 's'. If s=width, it's UB.
        // Our API requires s < width via DCHECK.
        // Logic check for s=0 and s=max.

        // Up(0) should be all ones
        EXPECT_EQ(IntervalUp8(0), 0xFF);
        // Up(7) should be top bit only (0x80)
        EXPECT_EQ(IntervalUp8(7), 0x80);

        // Down(0) should be bottom bit only (0x01)
        EXPECT_EQ(IntervalDown8(0), 0x01);
        // Down(7) should be all ones
        EXPECT_EQ(IntervalDown8(7), 0xFF);
    }

    TEST(EdgeCaseTest, SearchRangeCrossesWordBoundaries)
    {
        // Setup: 64-bit words.
        // Word 0: 0
        // Word 1: 0
        // Word 2: 1 (at global index 128)
        std::vector<uint64_t> data(4, 0);
        SetBit64(data.data(), 128);

        // Search starting exactly at 128
        EXPECT_EQ(LeastSignificantBitPosition64(data.data(), 128, 200), 128);

        // Search ending exactly at 128
        EXPECT_EQ(MostSignificantBitPosition64(data.data(), 0, 128), 128);

        // Search that encompasses it but start/end are in words 0 and 3
        EXPECT_EQ(LeastSignificantBitPosition64(data.data(), 10, 200), 128);
    }

    TEST(EdgeCaseTest, SearchRangeWithinSingleWordMasked)
    {
        // Word 0: 1111...1111
        const std::vector<uint64_t> data = {~0ULL};

        // Restrict search to [4, 5].
        // Should return 4 as LSB, 5 as MSB.
        EXPECT_EQ(LeastSignificantBitPosition64(data.data(), 4, 5), 4);
        EXPECT_EQ(MostSignificantBitPosition64(data.data(), 4, 5), 5);
    }

    TEST(EdgeCaseTest, SearchRangeNoBitsSetInSubset)
    {
        // Word 0: 11000011 (bits 0,1,6,7 set)
        constexpr uint8_t data[] = {0xC3}; // 11000011

        // Search in middle [2, 5] - should be empty
        EXPECT_EQ(LeastSignificantBitPosition8(data, 2, 5), -1);
        EXPECT_EQ(MostSignificantBitPosition8(data, 2, 5), -1);

        // Verify range count
        EXPECT_EQ(BitCountRange8(data, 2, 5), 0);
        EXPECT_TRUE(IsEmptyRange8(data, 2, 5));
    }

    TEST(EdgeCaseTest, DeBruijnManualFallback)
    {
        // Force test the fallback logic even if intrinsics are active
        for (int i = 0; i < 64; ++i)
        {
            const uint64_t val = 1ULL << i;
            EXPECT_EQ(LeastSignificantBitPosition64DeBruijn(val), i);
        }
    }

#ifndef NDEBUG
    TEST(DeathTest, NullPointerDetection)
    {
        uint64_t* null_ptr = nullptr;
        EXPECT_DEATH(IsBitSet64(null_ptr, 0), "Bitset pointer is null");
        EXPECT_DEATH(SetBit64(null_ptr, 0), "Bitset pointer is null");
        EXPECT_DEATH(ClearBit64(null_ptr, 0), "Bitset pointer is null");
        EXPECT_DEATH(BitCountRange64(null_ptr, 0, 10), "Bitset pointer is null");
        EXPECT_DEATH(LeastSignificantBitPosition64(null_ptr, 0, 10), "Bitset pointer is null");
    }

    TEST(DeathTest, InvalidShiftAmounts)
    {
        EXPECT_DEATH(BitMask64(64), "Shift amount must be less than");
        EXPECT_DEATH(BitMask8(8), "Shift amount must be less than");
    }

    TEST(DeathTest, InvalidRangeOrdering)
    {
        constexpr uint64_t arr[] = {0};
        // Start > End
        EXPECT_DEATH(BitCountRange64(arr, 10, 5), "Start position must be less than or equal to end");
        EXPECT_DEATH(IsEmptyRange64(arr, 10, 5), "Start position must be less than or equal to end");

        // Note: LSB/MSB search functions return -1 on start > end, they don't assert (design choice in impl).
        EXPECT_EQ(LeastSignificantBitPosition64(arr, 10, 5), -1);
    }

    TEST(DeathTest, ZeroInputForPositions)
    {
        // LSB/MSB position on value 0 is undefined
        EXPECT_DEATH(LeastSignificantBitPosition64(0), "LSB position is undefined");
        EXPECT_DEATH(MostSignificantBitPosition32(0), "MSB position is undefined");
    }
#endif

    // Reference implementation for validation
    static int64_t NaiveCountRange(const std::vector<uint64_t>& data, uint64_t start, uint64_t end)
    {
        int64_t count = 0;
        for (uint64_t i = start; i <= end; ++i)
        {
            if (IsBitSet64(data.data(), i))
            {
                count++;
            }
        }
        return count;
    }

    TEST(FuzzTest, BitCountRangeRandomized)
    {
        std::mt19937_64 rng(42); // NOLINT(*-msc51-cpp)
        std::uniform_int_distribution<uint64_t> val_dist;

        // Create 10 words (640 bits)
        std::vector<uint64_t> data(10);
        for (auto& val : data)
        {
            val = val_dist(rng);
        }

        std::uniform_int_distribution<uint64_t> idx_dist(0, 639);

        for (int i = 0; i < 500; ++i)
        {
            uint64_t s = idx_dist(rng);
            uint64_t e = idx_dist(rng);
            if (s > e)
            {
                std::swap(s, e);
            }

            const uint64_t expected = NaiveCountRange(data, s, e);
            const uint64_t actual = BitCountRange64(data.data(), s, e);

            ASSERT_EQ(expected, actual) << "Failed at range [" << s << ", " << e << "]";
        }
    }

    TEST(FuzzTest, MSBSearchRandomized)
    {
        std::mt19937_64 rng(1337); // NOLINT(*-msc51-cpp)
        std::uniform_int_distribution<uint64_t> val_dist;
        std::vector<uint64_t> data(4); // 256 bits

        for (int i = 0; i < 200; ++i)
        {
            for (auto& val : data)
            {
                val = val_dist(rng);
            }

            // Pick random range
            std::uniform_int_distribution<uint64_t> range_dist(0, 255);
            uint64_t s = range_dist(rng);
            uint64_t e = range_dist(rng);
            if (s > e)
            {
                std::swap(s, e);
            }

            // Manual find
            int64_t expected = -1;
            for (auto k = static_cast<int64_t>(e); k >= static_cast<int64_t>(s); --k)
            {
                if (IsBitSet64(data.data(), k))
                {
                    expected = k;
                    break;
                }
            }

            const int64_t actual = MostSignificantBitPosition64(data.data(), s, e);
            ASSERT_EQ(expected, actual) << "MSB Search failed range [" << s << ", " << e << "]";
        }
    }

    TEST(FuzzTest, LSBSearchRandomized)
    {
        std::mt19937_64 rng(9999); // NOLINT(*-msc51-cpp)
        std::uniform_int_distribution<uint64_t> val_dist;
        std::vector<uint64_t> data(4); // 256 bits

        for (int i = 0; i < 200; ++i)
        {
            for (auto& val : data)
            {
                val = val_dist(rng);
            }

            // Pick random range
            std::uniform_int_distribution<uint64_t> range_dist(0, 255);
            uint64_t s = range_dist(rng);
            uint64_t e = range_dist(rng);
            if (s > e)
            {
                std::swap(s, e);
            }

            // Manual find for LSB
            int64_t expected = -1;
            for (auto k = static_cast<int64_t>(s); k <= static_cast<int64_t>(e); ++k)
            {
                if (IsBitSet64(data.data(), k))
                {
                    expected = k;
                    break;
                }
            }

            const int64_t actual = LeastSignificantBitPosition64(data.data(), s, e);
            ASSERT_EQ(expected, actual) << "LSB Search failed range [" << s << ", " << e << "]";
        }
    }
} // namespace bslt::bits::test
