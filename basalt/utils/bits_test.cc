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

#include "gtest/gtest.h"
#include "basalt/utils/bits.h"

namespace bslt::bits
{
    TEST(LSBTest, LeastSignificantBitPosition64_PowersOfTwo)
    {
        // Test every single bit position from 0 to 63
        for (uint64_t i = 0; i < 64; ++i)
        {
            const uint64_t val = (1ULL << i);
            EXPECT_EQ(LeastSignificantBitPosition64(val), i) << "Failed at index " << i;
        }
    }

    TEST(LSBTest, LeastSignificantBitPosition64_ComplexValues)
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

    // ============================================================================
    // 2. Bit Position Tests (Pos & Mask)
    // ============================================================================

    TEST(BitPosTest, BitPos64)
    {
        EXPECT_EQ(BitPos64(0), 0);
        EXPECT_EQ(BitPos64(63), 63);
        EXPECT_EQ(BitPos64(64), 0); // Wrap around
        EXPECT_EQ(BitPos64(65), 1);
    }

    TEST(BitPosTest, BitPos32)
    {
        EXPECT_EQ(BitPos32(0), 0);
        EXPECT_EQ(BitPos32(31), 31);
        EXPECT_EQ(BitPos32(32), 0);
    }

    TEST(BitPosTest, BitPos16)
    {
        EXPECT_EQ(BitPos16(0), 0);
        EXPECT_EQ(BitPos16(15), 15);
        EXPECT_EQ(BitPos16(16), 0);
    }

    TEST(BitPosTest, BitPos8)
    {
        EXPECT_EQ(BitPos8(0), 0);
        EXPECT_EQ(BitPos8(7), 7);
        EXPECT_EQ(BitPos8(8), 0);
    }

    // ============================================================================
    // 3. Bit Offset Tests (Word Index)
    // ============================================================================

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

    // ============================================================================
    // 4. Bit Length Tests (Allocation Size)
    // ============================================================================

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
}
