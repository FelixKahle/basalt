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
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND;
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#include <sstream>
#include <vector>
#include <numeric>
#include <gtest/gtest.h>
#include "basalt/container/bitset.h"

namespace bslt::test
{
    template <typename T>
    class BitsetTest : public ::testing::Test
    {
    public:
        using StorageType = T;
        using BitsetT = Bitset<StorageType>;
    };

    // Define the list of types to test
    using MyTypes = ::testing::Types<uint8_t, uint16_t, uint32_t, uint64_t>;
    TYPED_TEST_SUITE(BitsetTest, MyTypes);

    TYPED_TEST(BitsetTest, DefaultConstruction)
    {
        using Bitset = TestFixture::BitsetT;
        Bitset b;
        EXPECT_EQ(b.size(), 0);
        EXPECT_TRUE(b.empty());
        EXPECT_EQ(b.capacity(), 0);
        EXPECT_EQ(b.Count(), 0);
    }

    TYPED_TEST(BitsetTest, SizedConstruction)
    {
        using Bitset = TestFixture::BitsetT;
        // False init
        Bitset b1(100, false);
        EXPECT_EQ(b1.size(), 100);
        EXPECT_FALSE(b1.Any());

        // True init
        Bitset b2(100, true);
        EXPECT_EQ(b2.size(), 100);
        EXPECT_EQ(b2.Count(), 100);
        EXPECT_TRUE(b2.All());
    }

    TYPED_TEST(BitsetTest, SetClearTest)
    {
        using Bitset = TestFixture::BitsetT;
        Bitset b(64); // Fits in 1 uint64, spans multiple uint8s

        b.Set(0);
        EXPECT_TRUE(b.Test(0));
        EXPECT_TRUE(b[0]);

        b.Set(63);
        EXPECT_TRUE(b.Test(63));
        EXPECT_EQ(b.Count(), 2);

        b.Clear(0);
        EXPECT_FALSE(b.Test(0));
        EXPECT_EQ(b.Count(), 1);

        b.Set(10, false);
        EXPECT_FALSE(b.Test(10));

        b.Set(10, true);
        EXPECT_TRUE(b.Test(10));
    }

    TYPED_TEST(BitsetTest, ProxyReferenceManipulation)
    {
        using Bitset = TestFixture::BitsetT;
        Bitset b(10);

        // Assignment to true
        b[5] = true;
        EXPECT_TRUE(b.Test(5));

        // Assignment to false
        b[5] = false;
        EXPECT_FALSE(b.Test(5));

        // Assignment from another reference
        b[0] = true;
        b[1] = b[0];
        EXPECT_TRUE(b.Test(1));

        // Flip via reference
        b[2] = true;
        b[2] = !b[2];
        EXPECT_FALSE(b.Test(2));
    }

    TYPED_TEST(BitsetTest, OutOfBoundsCheck)
    {
        // Note: The implementation uses DCHECK (debug check).
        // In Release builds, these won't throw/terminate.
        // Standard GTest EXPECT_DEATH is only useful if we are in Debug mode.
#ifndef NDEBUG
        using Bitset = TestFixture::BitsetT;
        Bitset b(10);
        EXPECT_DEATH((void)b.Test(11), "");
        EXPECT_DEATH(b.Set(11), "");
#endif
    }

    TYPED_TEST(BitsetTest, SetResetFlipAll)
    {
        using Bitset = TestFixture::BitsetT;
        Bitset b(127); // Prime number size to test padding bits

        b.SetAll();
        EXPECT_EQ(b.Count(), 127);
        EXPECT_TRUE(b.All());

        b.ResetAll();
        EXPECT_EQ(b.Count(), 0);
        EXPECT_TRUE(b.None());

        b.FlipAll();
        EXPECT_EQ(b.Count(), 127);
        EXPECT_TRUE(b.All());

        // Verify padding bits aren't messing up "All()" logic
        // Manually clear one bit
        b.Clear(0);
        EXPECT_FALSE(b.All());
    }

    TYPED_TEST(BitsetTest, ResizeLargerAndSmaller)
    {
        using Bitset = TestFixture::BitsetT;
        Bitset b(10, true); // 1111111111

        // Resize larger with false fill
        b.Resize(20, false);
        EXPECT_EQ(b.size(), 20);
        EXPECT_EQ(b.Count(), 10); // Should not have added set bits
        EXPECT_FALSE(b.Test(19));

        // Resize larger with true fill
        b.Resize(30, true);
        EXPECT_EQ(b.size(), 30);
        EXPECT_EQ(b.Count(), 20); // 10 original + 10 new
        EXPECT_TRUE(b.Test(29));

        // Resize smaller
        b.Resize(5);
        EXPECT_EQ(b.size(), 5);
        EXPECT_EQ(b.Count(), 5); // Truncated
    }

    TYPED_TEST(BitsetTest, ResizeMaintainsPaddingInvariant)
    {
        // This test ensures that when we resize, garbage bits in the last word
        // are correctly cleared. If not, Count() or FindFirstUnset() breaks.
        using Bitset = TestFixture::BitsetT;

        // 1. Create a bitset where the underlying storage has capacity
        Bitset b(128, true);
        // 2. Shrink it to leave "dirty" bits in memory
        b.Resize(65);
        // 3. Check if Count is correct (should be 65, not 128)
        EXPECT_EQ(b.Count(), 65);

        // 4. Flip All. If padding bits weren't cleared during resize,
        // they might interact weirdly.
        b.FlipAll();
        EXPECT_EQ(b.Count(), 0);
    }

    TYPED_TEST(BitsetTest, FindOperations)
    {
        using Bitset = TestFixture::BitsetT;
        Bitset b(100, false);

        EXPECT_EQ(b.FindFirst(), -1);
        EXPECT_EQ(b.FindFirstUnset(), 0);

        b.Set(10);
        b.Set(20);
        b.Set(99);

        EXPECT_EQ(b.FindFirst(), 10);
        EXPECT_EQ(b.FindNext(10), 20);
        EXPECT_EQ(b.FindNext(20), 99);
        EXPECT_EQ(b.FindNext(99), -1);

        // Test FindFirstUnset
        b.SetAll();
        EXPECT_EQ(b.FindFirstUnset(), -1);
        b.Clear(50);
        EXPECT_EQ(b.FindFirstUnset(), 50);
        b.Clear(0);
        EXPECT_EQ(b.FindFirstUnset(), 0);
    }

    TYPED_TEST(BitsetTest, ForEachSetBit)
    {
        using Bitset = TestFixture::BitsetT;
        Bitset b(100, false);
        const std::vector<size_t> expected = {2, 15, 63, 64, 90};

        for (auto idx : expected)
        {
            b.Set(idx);
        }

        std::vector<size_t> actual;
        b.ForEachSetBit([&](size_t idx)
        {
            actual.push_back(idx);
        });

        EXPECT_EQ(actual, expected);
    }

    TYPED_TEST(BitsetTest, ForEachUnsetBit)
    {
        using Bitset = TestFixture::BitsetT;

        {
            Bitset b(100, true); // Initialize to all 1s
            const std::vector<size_t> expected = {2, 15, 63, 64, 90, 99};

            // Clear specific bits (these should be detected)
            for (auto idx : expected)
            {
                b.Clear(idx);
            }


            std::vector<size_t> actual;
            b.ForEachUnsetBit([&](size_t idx)
            {
                actual.push_back(idx);
            });

            EXPECT_EQ(actual, expected);
        }

        {
            // Create a bitset that doesn't align perfectly with storage words.
            // E.g., if Storage is uint64_t, size 66 leaves 62 padding bits.
            // If logic is wrong, inverting 0-padding makes them 1, and they might be visited.
            Bitset b(66, true);
            b.Clear(65);

            std::vector<size_t> actual;
            b.ForEachUnsetBit([&](size_t idx) { actual.push_back(idx); });

            ASSERT_EQ(actual.size(), 1);
            EXPECT_EQ(actual[0], 65);
        }

        {
            Bitset b(5, false); // 00000
            std::vector<size_t> actual;
            b.ForEachUnsetBit([&](size_t idx) { actual.push_back(idx); });

            const std::vector<size_t> expected = {0, 1, 2, 3, 4};
            EXPECT_EQ(actual, expected);
        }

        {
            Bitset b(50, true);
            size_t count = 0;
            b.ForEachUnsetBit([&](size_t) { count++; });
            EXPECT_EQ(count, 0);
        }

        {
            Bitset b(0);
            bool visited = false;
            b.ForEachUnsetBit([&](size_t) { visited = true; });
            EXPECT_FALSE(visited);
        }
    }

    TYPED_TEST(BitsetTest, BitwiseOperators)
    {
        using Bitset = TestFixture::BitsetT;
        Bitset b1(4, false); // 0000
        Bitset b2(4, false); // 0000

        b1.Set(0);
        b1.Set(1); // 0011 (LSB at 0)
        b2.Set(1);
        b2.Set(2); // 0110

        Bitset band = b1;
        band &= b2; // 0010
        EXPECT_FALSE(band[0]);
        EXPECT_TRUE(band[1]);
        EXPECT_FALSE(band[2]);

        Bitset bor = b1;
        bor |= b2; // 0111
        EXPECT_TRUE(bor[0]);
        EXPECT_TRUE(bor[1]);
        EXPECT_TRUE(bor[2]);
        EXPECT_FALSE(bor[3]);

        Bitset bxor = b1;
        bxor ^= b2; // 0101
        EXPECT_TRUE(bxor[0]);
        EXPECT_FALSE(bxor[1]); // 1^1 = 0
        EXPECT_TRUE(bxor[2]);

        Bitset bnot = ~b1; // 1100
        EXPECT_FALSE(bnot[0]);
        EXPECT_FALSE(bnot[1]);
        EXPECT_TRUE(bnot[2]);
        EXPECT_TRUE(bnot[3]);
    }

    TYPED_TEST(BitsetTest, ShiftLeft)
    {
        using Bitset = TestFixture::BitsetT;
        Bitset b(10);
        b.Set(0);
        b.Set(1);
        // State: 0...0011

        // Small shift
        b <<= 1;
        EXPECT_FALSE(b[0]);
        EXPECT_TRUE(b[1]);
        EXPECT_TRUE(b[2]); // State: 0...0110

        // Large shift (block boundary crossing)
        Bitset large(128);
        large.Set(0);
        large.Set(127);

        large <<= 64; // Should move bit 0 to 64, bit 127 falls off
        EXPECT_FALSE(large[0]);
        EXPECT_TRUE(large[64]);
        EXPECT_FALSE(large[127]);

        // Shift clear
        large <<= 128;
        EXPECT_TRUE(large.None());
    }

    TYPED_TEST(BitsetTest, ShiftRight)
    {
        using Bitset = TestFixture::BitsetT;
        Bitset b(10);
        b.Set(8);
        b.Set(9);
        // State: 1100000000 (visualizing index 9 at left)

        b >>= 1;
        EXPECT_TRUE(b[7]);
        EXPECT_TRUE(b[8]);
        EXPECT_FALSE(b[9]);

        // Block crossing
        Bitset large(128);
        large.Set(64);
        large >>= 64;
        EXPECT_TRUE(large[0]);
        EXPECT_FALSE(large[64]);
    }

    TYPED_TEST(BitsetTest, IteratorTraversal)
    {
        using Bitset = TestFixture::BitsetT;
        Bitset b(5);
        b.Set(0);
        b.Set(2);
        b.Set(4); // 10101

        auto it = b.begin();
        EXPECT_TRUE(*it); // 0
        EXPECT_FALSE(*(++it)); // 1
        EXPECT_TRUE(*(++it)); // 2
        EXPECT_FALSE(*(++it)); // 3
        EXPECT_TRUE(*(++it)); // 4
        EXPECT_EQ(++it, b.end());
    }

    TYPED_TEST(BitsetTest, IteratorRandomAccess)
    {
        using Bitset = TestFixture::BitsetT;
        Bitset b(10, true); // All 1s

        auto it = b.begin();
        EXPECT_EQ(it + 10, b.end());
        EXPECT_EQ(b.end() - 10, b.begin());
        EXPECT_EQ(b.end() - b.begin(), 10);

        it += 5;
        EXPECT_EQ(it - b.begin(), 5);

        it -= 2;
        EXPECT_EQ(it - b.begin(), 3);
    }

    TYPED_TEST(BitsetTest, STLAlgorithmCompatibility)
    {
        using Bitset = TestFixture::BitsetT;
        Bitset b(10);

        // 1. std::fill
        std::fill(b.begin(), b.end(), true);
        EXPECT_TRUE(b.All());

        // 2. std::count
        b.Resize(10);
        b.ResetAll();
        b[0] = true;
        b[1] = true;
        EXPECT_EQ(std::count(b.begin(), b.end(), true), 2);

        // 3. std::reverse (Requires swap to work on Reference)
        b.ResetAll();
        b.Set(0); // 1000000000...
        std::reverse(b.begin(), b.end());
        EXPECT_FALSE(b[0]);
        EXPECT_TRUE(b[9]); // ...0000000001

        // 4. std::find
        auto it = std::find(b.begin(), b.end(), true);
        EXPECT_EQ(it - b.begin(), 9);
    }

    TYPED_TEST(BitsetTest, RangeBasedForLoop)
    {
        using Bitset = TestFixture::BitsetT;
        Bitset b(5);
        b.SetAll();

        size_t count = 0;
        for (auto bit : b)
        {
            if (bit) count++;
        }
        EXPECT_EQ(count, 5);

        // Modifying via range loop (using reference)
        // ReSharper disable once CppEntityAssignedButNoRead
        for (auto bit : b)
        {
            bit = false;
        }
        EXPECT_TRUE(b.None());
    }

    TYPED_TEST(BitsetTest, ConstIterator)
    {
        using Bitset = TestFixture::BitsetT;
        Bitset b(5, true);
        const Bitset& cb = b;

        auto it = cb.begin();
        EXPECT_TRUE(*it);
        // *it = false; // Should fail to compile

        EXPECT_EQ(std::count(cb.cbegin(), cb.cend(), true), 5);
    }

    TYPED_TEST(BitsetTest, StringRepresentation)
    {
        using Bitset = TestFixture::BitsetT;
        Bitset b(4);
        b.Set(0); // LSB
        b.Set(2);

        // Index 3 is 0, Index 2 is 1, Index 1 is 0, Index 0 is 1
        // String: "0101"
        EXPECT_EQ(b.ToString(), "0101");
    }

    TYPED_TEST(BitsetTest, StreamOperator)
    {
        using Bitset = TestFixture::BitsetT;
        Bitset b(4);
        b.Set(0);
        b.Set(3); // 1001

        std::stringstream ss;
        ss << b;

        // Index 3(1) 2(0) 1(0) 0(1) -> "1001"
        EXPECT_EQ(ss.str(), "1001");
    }

    TYPED_TEST(BitsetTest, Equality)
    {
        using Bitset = TestFixture::BitsetT;
        Bitset b1(10, true);
        Bitset b2(10, true);

        EXPECT_EQ(b1, b2);

        b1.Set(0, false);
        EXPECT_NE(b1, b2);

        Bitset b3(11, true);
        EXPECT_NE(b2, b3); // Diff size
    }
} // namespace bslt::test
