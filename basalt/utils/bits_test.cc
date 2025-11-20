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
#include "gtest/gtest.h"
#include "basalt/utils/bits.h"

namespace bslt::bits
{
    static_assert(LeastSignificantBitPosition64DeBruijn(1ULL << 0) == 0, "Constexpr LSB failed");
    static_assert(LeastSignificantBitPosition64DeBruijn(1ULL << 63) == 63, "Constexpr LSB failed");
    static_assert(BitPosition64(65) == 1, "Constexpr BitPos64 failed");
    static_assert(BitPosition32(33) == 1, "Constexpr BitPos32 failed");
    static_assert(BitOffset64(128) == 2, "Constexpr BitOffset64 failed");
    static_assert(BitLength64(65) == 2, "Constexpr BitLength64 failed");
    static_assert(BitLength32(33) == 2, "Constexpr BitLength32 failed");

    static_assert(kAllBits8 == 0xFF, "kAllBits8 incorrect");
    static_assert(kAllBitsButLsb8 == 0xFE, "kAllBitsButLsb8 incorrect");
    static_assert(OneBit64(0) == 1ULL, "OneBit64(0) failed");
    static_assert(OneBit64(63) == (1ULL << 63), "OneBit64(63) failed");
    static_assert(OneBit8(7) == 0x80, "OneBit8(7) failed");
    static_assert(BitCount64(0) == 0, "BitCount64(0) failed");
    static_assert(BitCount32(11) == 3, "BitCount32(11) failed");
    static_assert(BitCount64(kAllBits64) == 64, "BitCount64(MAX) failed");
    static_assert(BitCount16(0xFFFF) == 16, "BitCount16(MAX) failed");
    static_assert(BitCount16(0x0101) == 2, "BitCount16(0x0101) failed");
    static_assert(BitCount8(0xFF) == 8, "BitCount8(MAX) failed");
    static_assert(BitCount8(0x11) == 2, "BitCount8(0x11) failed");

    static_assert(OneBit<uint64_t>(10) == 1024, "Constexpr OneBit<u64> failed");
    static_assert(OneBit<uint32_t>(5) == 32, "Constexpr OneBit<u32> failed");
    static_assert(OneBit<uint16_t>(4) == 16, "Constexpr OneBit<u16> failed");
    static_assert(OneBit<uint8_t>(3) == 8, "Constexpr OneBit<u8> failed");

    static_assert(LeastSignificantBitWord64(0) == 0, "LSBWord64(0) failed");
    static_assert(LeastSignificantBitWord64(12) == 4, "LSBWord64(12) failed"); // 1100 -> 0100
    static_assert(LeastSignificantBitWord32(12) == 4, "LSBWord32(12) failed");
    static_assert(LeastSignificantBitWord16(12) == 4, "LSBWord16(12) failed");
    static_assert(LeastSignificantBitWord8(12) == 4, "LSBWord8(12) failed");
    static_assert(LeastSignificantBitWord<uint64_t>(24) == 8, "LSBWord Template failed");


    TEST(BitConstantsTest, AllBitsValues)
    {
        // Verify against std::numeric_limits max() which is the standard way to get all 1s
        EXPECT_EQ(kAllBits64, std::numeric_limits<uint64_t>::max());
        EXPECT_EQ(kAllBits32, std::numeric_limits<uint32_t>::max());
        EXPECT_EQ(kAllBits16, std::numeric_limits<uint16_t>::max());
        EXPECT_EQ(kAllBits8, std::numeric_limits<uint8_t>::max());
    }

    TEST(BitConstantsTest, AllBitsButLsbValues)
    {
        // Verify logic: Max value XOR 1 should turn off the LSB
        EXPECT_EQ(kAllBitsButLsb64, std::numeric_limits<uint64_t>::max() ^ 1ULL);
        EXPECT_EQ(kAllBitsButLsb32, std::numeric_limits<uint32_t>::max() ^ 1U);
        EXPECT_EQ(kAllBitsButLsb16, static_cast<uint16_t>(std::numeric_limits<uint16_t>::max() ^ 1));
        EXPECT_EQ(kAllBitsButLsb8, static_cast<uint8_t>(std::numeric_limits<uint8_t>::max() ^ 1));

        // Verify explicitly that LSB is 0 and others are 1
        EXPECT_EQ(kAllBitsButLsb8 & 1, 0);
        EXPECT_EQ(kAllBitsButLsb8 & 0x80, 0x80);
    }

    TEST(OneBitTest, OneBit64ShiftsCorrectly)
    {
        for (int32_t i = 0; i < 64; ++i)
        {
            uint64_t expected = 1ULL << i;
            EXPECT_EQ(OneBit64(i), expected) << "Failed at index " << i;
        }
    }

    TEST(OneBitTest, OneBit32ShiftsCorrectly)
    {
        for (int32_t i = 0; i < 32; ++i)
        {
            uint32_t expected = 1U << i;
            EXPECT_EQ(OneBit32(i), expected);
        }
    }

    TEST(OneBitTest, OneBit16ShiftsCorrectly)
    {
        for (int32_t i = 0; i < 16; ++i)
        {
            // Cast to avoid integer promotion issues during comparison
            auto expected = static_cast<uint16_t>(1U << i);
            EXPECT_EQ(OneBit16(i), expected);
        }
    }

    TEST(OneBitTest, OneBit8ShiftsCorrectly)
    {
        for (int32_t i = 0; i < 8; ++i)
        {
            auto expected = static_cast<uint8_t>(1U << i);
            EXPECT_EQ(OneBit8(i), expected);
        }
    }

    using UnsignedBitTypes = ::testing::Types<uint8_t, uint16_t, uint32_t, uint64_t>;

    template <typename T>
    class OneBitTemplateTest : public ::testing::Test
    {
    };

    TYPED_TEST_SUITE(OneBitTemplateTest, UnsignedBitTypes);

    TYPED_TEST(OneBitTemplateTest, GeneratesCorrectPowersOfTwo)
    {
        using T = TypeParam;

        // Calculate bit width of the current type (8, 16, 32, or 64)
        constexpr uint32_t kBitWidth = sizeof(T) * 8;

        for (uint32_t i = 0; i < kBitWidth; ++i)
        {
            // 1. Generate using the library function
            T actual = OneBit<T>(i);

            // 2. Generate using standard math (careful casting to ensure valid shift)
            // We use T{1} to ensure the '1' is the correct width before shifting.
            T expected = static_cast<T>(T{1} << i);

            EXPECT_EQ(actual, expected) << "Failed for type size " << sizeof(T) << " at bit index " << i;
        }
    }

    // Simple explicit tests to ensure the dispatch logic selects the right return type
    TEST(OneBitTemplateTest, ExplicitDispatchChecks)
    {
        // uint64_t
        auto val64 = OneBit<uint64_t>(63);
        EXPECT_EQ(val64, 0x8000000000000000ULL);
        static_assert(std::is_same_v<decltype(val64), uint64_t>);

        // uint32_t
        auto val32 = OneBit<uint32_t>(31);
        EXPECT_EQ(val32, 0x80000000U);
        static_assert(std::is_same_v<decltype(val32), uint32_t>);

        // uint16_t
        auto val16 = OneBit<uint16_t>(15);
        EXPECT_EQ(val16, 0x8000U);
        static_assert(std::is_same_v<decltype(val16), uint16_t>);

        // uint8_t
        auto val8 = OneBit<uint8_t>(7);
        EXPECT_EQ(val8, 0x80U);
        static_assert(std::is_same_v<decltype(val8), uint8_t>);
    }

    TEST(BitCountTest, BitCount64BasicPatterns)
    {
        EXPECT_EQ(BitCount64(0ULL), 0);
        EXPECT_EQ(BitCount64(kAllBits64), 64);
        EXPECT_EQ(BitCount64(0xAAAAAAAAAAAAAAAAULL), 32); // Alternating 1010...
        EXPECT_EQ(BitCount64(0x5555555555555555ULL), 32); // Alternating 0101...
        EXPECT_EQ(BitCount64(0x8000000000000001ULL), 2); // Edges
    }

    TEST(BitCountTest, BitCount32BasicPatterns)
    {
        EXPECT_EQ(BitCount32(0U), 0);
        EXPECT_EQ(BitCount32(kAllBits32), 32);
        EXPECT_EQ(BitCount32(0xAAAAAAAAU), 16);
        EXPECT_EQ(BitCount32(0x55555555U), 16);
        EXPECT_EQ(BitCount32(0x80000001U), 2);
    }

    TEST(BitCountTest, BitCount64Randomized)
    {
        // Compare against std::bitset (Ground Truth)
        std::mt19937_64 rng(12345); // NOLINT(*-msc51-cpp)
        std::uniform_int_distribution<uint64_t> dist;

        for (int i = 0; i < 1000; ++i)
        {
            uint64_t val = dist(rng);
            uint64_t actual = BitCount64(val);
            uint64_t expected = std::bitset<64>(val).count();
            ASSERT_EQ(actual, expected) << "Failed for value: " << val;
        }
    }

    TEST(BitCountTest, BitCount32Randomized)
    {
        std::mt19937 rng(54321); // NOLINT(*-msc51-cpp)
        std::uniform_int_distribution<uint32_t> dist;

        for (int i = 0; i < 1000; ++i)
        {
            uint32_t val = dist(rng);
            uint32_t actual = BitCount32(val);
            // bitset count returns size_t, cast to match
            uint32_t expected = static_cast<uint32_t>(std::bitset<32>(val).count());

            ASSERT_EQ(actual, expected) << "Failed for value: " << val;
        }
    }

    TEST(BitCountTest, BitCount16BasicPatterns)
    {
        EXPECT_EQ(BitCount16(0U), 0);
        EXPECT_EQ(BitCount16(0xFFFFU), 16);
        EXPECT_EQ(BitCount16(0xAAAAU), 8); // 1010...
        EXPECT_EQ(BitCount16(0x5555U), 8); // 0101...
        EXPECT_EQ(BitCount16(0x8001U), 2); // Edges
        EXPECT_EQ(BitCount16(0x00FFU), 8); // Lower byte
        EXPECT_EQ(BitCount16(0xFF00U), 8); // Upper byte
    }

    TEST(BitCountTest, BitCount8BasicPatterns)
    {
        EXPECT_EQ(BitCount8(0U), 0);
        EXPECT_EQ(BitCount8(0xFFU), 8);
        EXPECT_EQ(BitCount8(0xAAU), 4); // 1010...
        EXPECT_EQ(BitCount8(0x55U), 4); // 0101...
        EXPECT_EQ(BitCount8(0x81U), 2); // Edges
        EXPECT_EQ(BitCount8(0x0FU), 4); // Lower nibble
        EXPECT_EQ(BitCount8(0xF0U), 4); // Upper nibble
    }

    TEST(BitCountTest, BitCount16Exhaustive)
    {
        // 16-bit is small enough to test EVERY value
        for (uint32_t i = 0; i <= 0xFFFF; ++i)
        {
            const auto val = static_cast<uint16_t>(i);
            EXPECT_EQ(BitCount16(val), std::bitset<16>(val).count()) << "Failed for value: " << i;
        }
    }

    TEST(BitCountTest, BitCount8Exhaustive)
    {
        // 8-bit is small enough to test EVERY value
        for (uint32_t i = 0; i <= 0xFF; ++i)
        {
            const auto val = static_cast<uint8_t>(i);
            EXPECT_EQ(BitCount8(val), std::bitset<8>(val).count()) << "Failed for value: " << i;
        }
    }

    TEST(LSBTest, ExplicitDeBruijnLogic)
    {
        // Verify the specific lookup table logic works for all bits
        for (int i = 0; i < 64; ++i)
        {
            const uint64_t val = 1ULL << i;
            EXPECT_EQ(LeastSignificantBitPosition64DeBruijn(val), i)
                << "DeBruijn lookup failed at bit " << i;
        }

        // Verify it handles complex masking correctly (LSB isolation)
        // 0b10100 -> LSB is at index 2 (value 4)
        EXPECT_EQ(LeastSignificantBitPosition64DeBruijn(20ULL), 2);
    }

    TEST(LSBWordTest, Isolation64)
    {
        // 0 -> 0
        EXPECT_EQ(LeastSignificantBitWord64(0), 0);

        // 12 (1100) -> 4 (0100)
        EXPECT_EQ(LeastSignificantBitWord64(12), 4);

        // All ones -> 1 (lowest bit is set)
        EXPECT_EQ(LeastSignificantBitWord64(0xFFFFFFFFFFFFFFFFULL), 1);
        // High bit only -> High bit
        EXPECT_EQ(LeastSignificantBitWord64(0x8000000000000000ULL), 0x8000000000000000ULL);
    }

    TEST(LSBWordTest, Isolation32)
    {
        EXPECT_EQ(LeastSignificantBitWord32(0), 0);
        EXPECT_EQ(LeastSignificantBitWord32(12), 4);
        EXPECT_EQ(LeastSignificantBitWord32(0xFFFFFFFFU), 1);
    }

    TEST(LSBWordTest, Isolation16)
    {
        EXPECT_EQ(LeastSignificantBitWord16(0), 0);
        EXPECT_EQ(LeastSignificantBitWord16(12), 4);
        EXPECT_EQ(LeastSignificantBitWord16(0xFFFF), 1);

        // Check upper byte handling
        EXPECT_EQ(LeastSignificantBitWord16(0x00F0), 0x0010);
    }

    TEST(LSBWordTest, Isolation8)
    {
        EXPECT_EQ(LeastSignificantBitWord8(0), 0);
        EXPECT_EQ(LeastSignificantBitWord8(12), 4);
        EXPECT_EQ(LeastSignificantBitWord8(0xFF), 1);
        EXPECT_EQ(LeastSignificantBitWord8(0xF0), 0x10);
    }

    template <typename T>
    class LSBWordTemplateTest : public ::testing::Test
    {
    };

    TYPED_TEST_SUITE(LSBWordTemplateTest, UnsignedBitTypes);

    TYPED_TEST(LSBWordTemplateTest, GenericIsolation)
    {
        using T = TypeParam;
        EXPECT_EQ(LeastSignificantBitWord<T>(0), 0);
        constexpr uint32_t kBits = sizeof(T) * 8;
        for (uint32_t i = 0; i < kBits; ++i)
        {
            T val = static_cast<T>(T{1} << i);
            EXPECT_EQ(LeastSignificantBitWord<T>(val), val) << "Failed identity check for power of 2 at bit " << i;
        }

        if (sizeof(T) * 8 >= 4)
        {
            EXPECT_EQ(LeastSignificantBitWord<T>(static_cast<T>(12)), static_cast<T>(4));
        }
    }

    TEST(LSBTest, LeastSignificantBitPosition64PowersOfTwo)
    {
        // Test every single bit position from 0 to 63
        for (uint64_t i = 0; i < 64; ++i)
        {
            const uint64_t val = (1ULL << i);
            EXPECT_EQ(LeastSignificantBitPosition64(val), i) << "Failed at index " << i;
        }
    }

    TEST(LSBTest, LeastSignificantBitPosition64ComplexValues)
    {
        // LSB of 12 (binary 1100) is 2 (binary 100 is 4)
        EXPECT_EQ(LeastSignificantBitPosition64(12), 2);
        // LSB of ...10000 is 4
        EXPECT_EQ(LeastSignificantBitPosition64(0xFFFFFFFFFFFFFF10ULL), 4);
    }

    TEST(LSBTest, LeastSignificantBitPosition32)
    {
        for (uint32_t i = 0; i < 32; ++i)
        {
            const uint32_t val = (1U << i);
            EXPECT_EQ(LeastSignificantBitPosition32(val), i);
        }
        // Test upper bound logic fallback
        EXPECT_EQ(LeastSignificantBitPosition32(0x80000000), 31);
    }

    TEST(LSBTest, LeastSignificantBitPosition16)
    {
        for (uint16_t i = 0; i < 16; ++i)
        {
            const auto val = static_cast<uint16_t>(1 << i);
            EXPECT_EQ(LeastSignificantBitPosition16(val), i);
        }
    }

    TEST(LSBTest, LeastSignificantBitPosition8)
    {
        for (uint8_t i = 0; i < 8; ++i)
        {
            const auto val = static_cast<uint8_t>(1 << i);
            EXPECT_EQ(LeastSignificantBitPosition8(val), i);
        }
    }

    TEST(BitPosTest, BitPos64)
    {
        EXPECT_EQ(BitPosition64(0), 0);
        EXPECT_EQ(BitPosition64(63), 63);
        EXPECT_EQ(BitPosition64(64), 0); // Wrap around
        EXPECT_EQ(BitPosition64(65), 1);
    }

    TEST(BitPosTest, BitPos32)
    {
        EXPECT_EQ(BitPosition32(0), 0);
        EXPECT_EQ(BitPosition32(31), 31);
        EXPECT_EQ(BitPosition32(32), 0);
    }

    TEST(BitPosTest, BitPos16)
    {
        EXPECT_EQ(BitPosition16(0), 0);
        EXPECT_EQ(BitPosition16(15), 15);
        EXPECT_EQ(BitPosition16(16), 0);
    }

    TEST(BitPosTest, BitPos8)
    {
        EXPECT_EQ(BitPosition8(0), 0);
        EXPECT_EQ(BitPosition8(7), 7);
        EXPECT_EQ(BitPosition8(8), 0);
    }

    TEST(BitOffsetTest, BitOffset64)
    {
        EXPECT_EQ(BitOffset64(0), 0);
        EXPECT_EQ(BitOffset64(63), 0);
        EXPECT_EQ(BitOffset64(64), 1); // New word
        EXPECT_EQ(BitOffset64(127), 1);
        EXPECT_EQ(BitOffset64(128), 2);
    }

    TEST(BitOffsetTest, BitOffset32)
    {
        EXPECT_EQ(BitOffset32(31), 0);
        EXPECT_EQ(BitOffset32(32), 1);
    }

    TEST(BitOffsetTest, BitOffset16)
    {
        EXPECT_EQ(BitOffset16(15), 0);
        EXPECT_EQ(BitOffset16(16), 1);
    }

    TEST(BitOffsetTest, BitOffset8)
    {
        EXPECT_EQ(BitOffset8(7), 0);
        EXPECT_EQ(BitOffset8(8), 1);
    }

    TEST(BitLengthTest, BitLength64)
    {
        EXPECT_EQ(BitLength64(0), 0);
        EXPECT_EQ(BitLength64(1), 1);
        EXPECT_EQ(BitLength64(64), 1);
        EXPECT_EQ(BitLength64(65), 2); // Needs 2nd uint64
    }

    TEST(BitLengthTest, BitLength32)
    {
        EXPECT_EQ(BitLength32(0), 0);
        EXPECT_EQ(BitLength32(32), 1);
        EXPECT_EQ(BitLength32(33), 2);

        // Test Large Input (Regression check for input type)
        // 200 bits needs: ceil(200/32) = 7 uint32s
        EXPECT_EQ(BitLength32(200), 7);
    }

    TEST(BitLengthTest, BitLength16)
    {
        EXPECT_EQ(BitLength16(16), 1);
        EXPECT_EQ(BitLength16(17), 2);
    }

    TEST(BitLengthTest, BitLength8)
    {
        EXPECT_EQ(BitLength8(8), 1);
        EXPECT_EQ(BitLength8(9), 2);

        // IMPORTANT: Test input larger than uint8_t max (255)
        // If the input parameter was uint8_t, this would overflow/wrap.
        // 1000 bits / 8 bits per byte = 125 bytes
        EXPECT_EQ(BitLength8(1000), 125);

        // 1001 bits / 8 = 125.125 -> needs 126 bytes
        EXPECT_EQ(BitLength8(1001), 126);
    }

    TEST(LSBTest, BitPatterns)
    {
        // All ones
        EXPECT_EQ(LeastSignificantBitPosition64(0xFFFFFFFFFFFFFFFFULL), 0);
        EXPECT_EQ(LeastSignificantBitPosition32(0xFFFFFFFFU), 0);

        // Alternating bits 1010... (LSB is at index 1)
        EXPECT_EQ(LeastSignificantBitPosition64(0xAAAAAAAAAAAAAAAAULL), 1);

        // Alternating bits 0101... (LSB is at index 0)
        EXPECT_EQ(LeastSignificantBitPosition64(0x5555555555555555ULL), 0);

        // High bit set combined with low bit
        // (1 << 63) | (1 << 3) -> LSB is 3
        EXPECT_EQ(LeastSignificantBitPosition64((1ULL << 63) | (1ULL << 3)), 3);
    }

    TEST(BitLengthTest, ExactMultiples)
    {
        // 64 bits fits exactly in 1 uint64
        EXPECT_EQ(BitLength64(64), 1);
        // 65 bits rolls over to 2
        EXPECT_EQ(BitLength64(65), 2);
        // 128 bits fits exactly in 2 uint64s
        EXPECT_EQ(BitLength64(128), 2);
        // 129 bits rolls over to 3
        EXPECT_EQ(BitLength64(129), 3);
    }

    TEST(BitLengthTest, ZeroInput)
    {
        // While LSB forbids 0, Length usually allows size 0
        EXPECT_EQ(BitLength64(0), 0);
        EXPECT_EQ(BitLength32(0), 0);
        EXPECT_EQ(BitLength16(0), 0);
        EXPECT_EQ(BitLength8(0), 0);
    }

    TEST(LSBTest, ZeroInputDeathTest)
    {
#ifndef NDEBUG
        EXPECT_DEATH(LeastSignificantBitPosition64(0), "LSB position is undefined for value 0");
        EXPECT_DEATH(LeastSignificantBitPosition32(0), "LSB position is undefined for value 0");
#endif
    }

    // Helper for verification (if not already defined)
    static int NaiveLSB64(uint64_t n)
    {
        if (n == 0) return 0;
        for (int i = 0; i < 64; ++i)
        {
            if ((n >> i) & 1) return i;
        }
        return 64;
    }

    TEST(LSBTest, RandomizedCheck)
    {
        // Use a fixed seed for reproducibility (deterministic test)
        std::mt19937_64 rng(42); // NOLINT(*-msc51-cpp)

        // Distribution: [1, UINT64_MAX]
        // We start at 1 to ensure we never pass 0 to the LSB function,
        // avoiding the need for an explicit if(val == 0) check inside the loop.
        std::uniform_int_distribution<uint64_t> dist(1, std::numeric_limits<uint64_t>::max());

        for (int i = 0; i < 1000; ++i)
        {
            const uint64_t val = dist(rng);

            const uint64_t expected = NaiveLSB64(val);
            const uint64_t actual = LeastSignificantBitPosition64(val);

            ASSERT_EQ(expected, actual) << "Mismatch on value: " << val;
        }
    }

    TEST(TemplateTest, LeastSignificantBitPositionDispatch)
    {
        // Test uint64_t path
        EXPECT_EQ(LeastSignificantBitPosition<uint64_t>(1ULL << 60), 60);

        // Test uint32_t path
        EXPECT_EQ(LeastSignificantBitPosition<uint32_t>(1U << 30), 30);

        // Test uint16_t path
        EXPECT_EQ(LeastSignificantBitPosition<uint16_t>(static_cast<uint16_t>(1 << 14)), 14);

        // Test uint8_t path
        EXPECT_EQ(LeastSignificantBitPosition<uint8_t>(static_cast<uint8_t>(1 << 6)), 6);
    }

    TEST(TemplateTest, BitPositionDispatch)
    {
        // uint64_t: mod 64
        EXPECT_EQ(BitPosition<uint64_t>(65), 1);

        // uint32_t: mod 32
        EXPECT_EQ(BitPosition<uint32_t>(33), 1);

        // uint16_t: mod 16
        EXPECT_EQ(BitPosition<uint16_t>(17), 1);

        // uint8_t: mod 8
        EXPECT_EQ(BitPosition<uint8_t>(9), 1);
    }

    TEST(TemplateTest, BitOffsetDispatch)
    {
        // uint64_t: div 64
        EXPECT_EQ(BitOffset<uint64_t>(128), 2);

        // uint32_t: div 32
        EXPECT_EQ(BitOffset<uint32_t>(64), 2);

        // uint16_t: div 16
        EXPECT_EQ(BitOffset<uint16_t>(32), 2);

        // uint8_t: div 8
        EXPECT_EQ(BitOffset<uint8_t>(16), 2);
    }

    TEST(TemplateTest, BitLengthDispatch)
    {
        // Note: BitLength<T> templates on the RETURN type, input is always uint64_t size

        // Return uint64_t (blocks of 64 bits)
        EXPECT_EQ(BitLength<uint64_t>(129), 3);
        // Return uint32_t (blocks of 32 bits)
        EXPECT_EQ(BitLength<uint32_t>(65), 3);

        // Return uint8_t (blocks of 8 bits)
        EXPECT_EQ(BitLength<uint8_t>(17), 3);
    }

    // Define the types we want to test
    using UnsignedTypes = ::testing::Types<uint8_t, uint16_t, uint32_t, uint64_t>;

    // Create a test fixture class template
    template <typename T>
    class BitsTypedSwapTest : public ::testing::Test
    {
    };

    // Register the types
    TYPED_TEST_SUITE(BitsTypedSwapTest, UnsignedTypes);

    TYPED_TEST(BitsTypedSwapTest, LSBGenericLogic)
    {
        using T = TypeParam; // T is the current type being tested

        // 1. Test LSB for powers of 2
        // Limit the loop to the number of bits in type T
        constexpr int kBits = sizeof(T) * 8;

        for (int i = 0; i < kBits; ++i)
        {
            // Construct 1 << i carefully to avoid overflow warnings during shift
            T val = static_cast<T>(T{1} << i);
            EXPECT_EQ(LeastSignificantBitPosition<T>(val), i)
                << "Failed for type size " << sizeof(T) << " at bit " << i;
        }

        // 2. Test non-zero requirement death test (Debug only)
#ifndef NDEBUG
        EXPECT_DEATH(LeastSignificantBitPosition<T>(0), "LSB position is undefined");
#endif
    }

    TYPED_TEST(BitsTypedSwapTest, BitPositionGenericLogic)
    {
        using T = TypeParam;
        constexpr uint32_t kBits = sizeof(T) * 8;

        // At index 0, position is 0
        EXPECT_EQ(BitPosition<T>(0), 0);
        // At index (Bits - 1), position is (Bits - 1)
        EXPECT_EQ(BitPosition<T>(static_cast<T>(kBits - 1)), kBits - 1);

        // At index (Bits), position wraps to 0 (if T is large enough to hold Bits)
        // We cast to T; if T is uint8_t (max 255), 8 fits.
        // If we tested uint8_t with value 256, it would wrap naturally,
        // but here we are testing the BitPosition logic specifically.

        T exactWrap = static_cast<T>(kBits);
        if (exactWrap == kBits) // Ensure no overflow during assignment
        {
            EXPECT_EQ(BitPosition<T>(exactWrap), 0);
        }
    }

    TYPED_TEST(BitsTypedSwapTest, BitOffsetGenericLogic)
    {
        using T = TypeParam;
        constexpr int kBits = sizeof(T) * 8;

        // Index 0 -> Word 0
        EXPECT_EQ(BitOffset<T>(0), 0);

        // Index (kBits - 1) -> Word 0
        EXPECT_EQ(BitOffset<T>(static_cast<T>(kBits - 1)), 0);

        // Index (kBits) -> Word 1
        T nextWordIndex = static_cast<T>(kBits);
        EXPECT_EQ(BitOffset<T>(nextWordIndex), 1);
    }
}
