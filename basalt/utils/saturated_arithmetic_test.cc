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

#include <limits>
#include <type_traits>
#include "gtest/gtest.h"
#include "basalt/utils/saturated_arithmetic.h"

namespace bslt::test
{
    template <typename T>
    class SaturatedArithmeticTest : public ::testing::Test
    {
    protected:
        static constexpr T kMin = std::numeric_limits<T>::min();
        static constexpr T kMax = std::numeric_limits<T>::max();
        static constexpr T kZero = T{0};
        static constexpr T kOne = T{1};
        static constexpr T kTwo = T{2};

        // For signed overflow tests: max/2 is useful
        static constexpr T kHalfMax = kMax / 2;
    };

    // Testing all standard integral types: 8, 16, 32, 64 bit, signed and unsigned.
    using SaturatedArithmeticTypes = ::testing::Types<
        int8_t, int16_t, int32_t, int64_t,
        uint8_t, uint16_t, uint32_t, uint64_t>;

    TYPED_TEST_SUITE(SaturatedArithmeticTest, SaturatedArithmeticTypes);

    // -----------------------------------------------------------------------
    // Utility Checks
    // -----------------------------------------------------------------------

    TYPED_TEST(SaturatedArithmeticTest, AtMinOrMax)
    {
        EXPECT_TRUE(bslt::AtMinOrMax(TestFixture::kMin));
        EXPECT_TRUE(bslt::AtMinOrMax(TestFixture::kMax));

        // For unsigned types, kMin is 0, so AtMinOrMax(0) is true.
        // For signed types, 0 is neither min nor max.
        if constexpr (std::is_signed_v<TypeParam>)
        {
            EXPECT_FALSE(bslt::AtMinOrMax(TestFixture::kZero));
        }
        else
        {
            EXPECT_TRUE(bslt::AtMinOrMax(TestFixture::kZero));
        }

        // 1 is usually not min or max (unless type is 1-bit, not applicable here)
        EXPECT_FALSE(bslt::AtMinOrMax(TestFixture::kOne));
    }

    TYPED_TEST(SaturatedArithmeticTest, AddOverflows)
    {
        // No overflow
        EXPECT_FALSE(bslt::AddOverflows(TestFixture::kOne, TestFixture::kOne));
        EXPECT_FALSE(bslt::AddOverflows(TestFixture::kZero, TestFixture::kMax));

        // Overflow
        EXPECT_TRUE(bslt::AddOverflows(TestFixture::kMax, TestFixture::kOne));

        if constexpr (std::is_signed_v<TypeParam>)
        {
            // Underflow (Negative Overflow)
            EXPECT_TRUE(bslt::AddOverflows(TestFixture::kMin, static_cast<TypeParam>(-1)));
        }
    }

    TYPED_TEST(SaturatedArithmeticTest, SubOverflows)
    {
        // No overflow
        EXPECT_FALSE(bslt::SubOverflows(TestFixture::kMax, TestFixture::kOne));
        EXPECT_FALSE(bslt::SubOverflows(TestFixture::kOne, TestFixture::kOne));

        if constexpr (std::is_unsigned_v<TypeParam>)
        {
            // Unsigned wrap-around (0 - 1) is detected as overflow
            EXPECT_TRUE(bslt::SubOverflows(TestFixture::kZero, TestFixture::kOne));
        }
        else
        {
            // Signed: Max - (-1) = Overflow
            EXPECT_TRUE(bslt::SubOverflows(TestFixture::kMax, static_cast<TypeParam>(-1)));
            // Signed: Min - 1 = Underflow
            EXPECT_TRUE(bslt::SubOverflows(TestFixture::kMin, TestFixture::kOne));
        }
    }

    // -----------------------------------------------------------------------
    // Saturated Casting
    // -----------------------------------------------------------------------

    TYPED_TEST(SaturatedArithmeticTest, CapCast)
    {
        // 1. Same type cast (Identity)
        EXPECT_EQ(bslt::CapCast<TypeParam>(TestFixture::kMax), TestFixture::kMax);
        EXPECT_EQ(bslt::CapCast<TypeParam>(TestFixture::kMin), TestFixture::kMin);

        // 2. Larger (int64_t) to TypeParam (Narrowing)
        int64_t huge = std::numeric_limits<int64_t>::max();
        if constexpr (sizeof(TypeParam) < 8 || (sizeof(TypeParam) == 8 && std::is_signed_v<TypeParam>))
        {
            // int64_max fits in uint64, fits in int64.
            // If TypeParam is smaller, or TypeParam is int64 (identity), this block runs.
            // Actually, if TypeParam is uint64, int64_max fits fine.
            // If TypeParam is smaller than 64 bit, it should saturate.
            if constexpr (sizeof(TypeParam) < 8)
            {
                EXPECT_EQ(bslt::CapCast<TypeParam>(huge), TestFixture::kMax);
            }
        }

        int64_t tiny = std::numeric_limits<int64_t>::min();
        if constexpr (std::is_unsigned_v<TypeParam>)
        {
            // Negative -> 0 (Min of unsigned)
            EXPECT_EQ(bslt::CapCast<TypeParam>(tiny), TestFixture::kMin);
            EXPECT_EQ(bslt::CapCast<TypeParam>(-1), TestFixture::kMin);
        }
        else
        {
            // Signed narrowing
            if constexpr (sizeof(TypeParam) < 8)
            {
                EXPECT_EQ(bslt::CapCast<TypeParam>(tiny), TestFixture::kMin);
            }
        }

        // 3. Unsigned -> Signed conversion
        // e.g., uint8(255) -> int8(127)
        if constexpr (std::is_signed_v<TypeParam>)
        {
            using UnsignedVer = std::make_unsigned_t<TypeParam>;
            auto u_max = std::numeric_limits<UnsignedVer>::max();
            EXPECT_EQ(bslt::CapCast<TypeParam>(u_max), TestFixture::kMax);
        }
    }

    // -----------------------------------------------------------------------
    // Unary Operations (Signed Only)
    // -----------------------------------------------------------------------

    TYPED_TEST(SaturatedArithmeticTest, CapOpp)
    {
        if constexpr (std::is_signed_v<TypeParam>)
        {
            // Normal
            EXPECT_EQ(bslt::CapOpp(TestFixture::kOne), static_cast<TypeParam>(-1));
            // Saturation: -Min -> Max
            EXPECT_EQ(bslt::CapOpp(TestFixture::kMin), TestFixture::kMax);
        }
    }

    TYPED_TEST(SaturatedArithmeticTest, CapAbs)
    {
        if constexpr (std::is_signed_v<TypeParam>)
        {
            // Normal
            EXPECT_EQ(bslt::CapAbs(static_cast<TypeParam>(-1)), TestFixture::kOne);
            // Saturation: Abs(Min) -> Max
            EXPECT_EQ(bslt::CapAbs(TestFixture::kMin), TestFixture::kMax);
        }
    }

    // -----------------------------------------------------------------------
    // Binary Operations: Add, Sub, Mul
    // -----------------------------------------------------------------------

    TYPED_TEST(SaturatedArithmeticTest, CapAdd)
    {
        // Normal
        EXPECT_EQ(bslt::CapAdd(TestFixture::kOne, TestFixture::kOne), TestFixture::kTwo);

        // Saturation High
        EXPECT_EQ(bslt::CapAdd(TestFixture::kMax, TestFixture::kOne), TestFixture::kMax);

        // Saturation Low (Signed)
        if constexpr (std::is_signed_v<TypeParam>)
        {
            EXPECT_EQ(bslt::CapAdd(TestFixture::kMin, static_cast<TypeParam>(-1)), TestFixture::kMin);
        }
    }

    TYPED_TEST(SaturatedArithmeticTest, CapSub)
    {
        // Normal
        EXPECT_EQ(bslt::CapSub(TestFixture::kTwo, TestFixture::kOne), TestFixture::kOne);

        // Saturation Low (Signed) / Zero (Unsigned logic currently saturates to Max on wrap for CapSub? No, overflow check!)
        // Wait, SubOverflows logic for unsigned: (0 - 1) wraps to huge.
        // CapSub implementation: if overflowed, return CapWithSignOf(x).
        // For unsigned x=0, CapWithSignOf(0) -> Max.
        // So 0 - 1 = Max Saturated at top of range because unsigned cannot go negative,
        // but traditionally saturation arithmetic clamps 0-1 to 0.
        // HOWEVER, the implementation provided clamps to MAX on overflow for unsigned types
        // because "overflow" is defined via builtins/wrap detection.
        // Let's verify specific implementation behavior:

        if constexpr (std::is_unsigned_v<TypeParam>)
        {
            // 0 - 1 wraps to Max. Overflow detected.
            // CapWithSignOf(0) returns Max.
            // Result is Max.
            EXPECT_EQ(bslt::CapSub(TestFixture::kZero, TestFixture::kOne), TestFixture::kMax);
        }
        else
        {
            // Signed: Min - 1 -> Min
            EXPECT_EQ(bslt::CapSub(TestFixture::kMin, TestFixture::kOne), TestFixture::kMin);
            // Signed: Max - (-1) -> Max
            EXPECT_EQ(bslt::CapSub(TestFixture::kMax, static_cast<TypeParam>(-1)), TestFixture::kMax);
        }
    }

    TYPED_TEST(SaturatedArithmeticTest, CapMul)
    {
        // Normal
        EXPECT_EQ(bslt::CapMul(TestFixture::kTwo, TestFixture::kTwo), static_cast<TypeParam>(4));

        // Saturation
        EXPECT_EQ(bslt::CapMul(TestFixture::kMax, TestFixture::kTwo), TestFixture::kMax);

        if constexpr (std::is_signed_v<TypeParam>)
        {
            // Min * 2 -> Min
            EXPECT_EQ(bslt::CapMul(TestFixture::kMin, TestFixture::kTwo), TestFixture::kMin);
            // Min * -1 -> Max
            EXPECT_EQ(bslt::CapMul(TestFixture::kMin, static_cast<TypeParam>(-1)), TestFixture::kMax);
        }
    }

    // -----------------------------------------------------------------------
    // Division
    // -----------------------------------------------------------------------

    TYPED_TEST(SaturatedArithmeticTest, CapDiv)
    {
        // Normal
        EXPECT_EQ(bslt::CapDiv(static_cast<TypeParam>(10), static_cast<TypeParam>(2)), static_cast<TypeParam>(5));

        if constexpr (std::is_signed_v<TypeParam>)
        {
            // The SIGFPE Case: Min / -1 -> Max
            EXPECT_EQ(bslt::CapDiv(TestFixture::kMin, static_cast<TypeParam>(-1)), TestFixture::kMax);

            // Other cases
            EXPECT_EQ(bslt::CapDiv(TestFixture::kMin, TestFixture::kOne), TestFixture::kMin);
        }
    }

    // -----------------------------------------------------------------------
    // In-Place Operations
    // -----------------------------------------------------------------------

    TYPED_TEST(SaturatedArithmeticTest, CapAddTo)
    {
        TypeParam v = TestFixture::kMax;
        bslt::CapAddTo(v, TestFixture::kOne);
        EXPECT_EQ(v, TestFixture::kMax); // Saturated

        v = TestFixture::kOne;
        bslt::CapAddTo(v, TestFixture::kOne);
        EXPECT_EQ(v, TestFixture::kTwo);
    }

    TYPED_TEST(SaturatedArithmeticTest, CapSubFrom)
    {
        TypeParam v = TestFixture::kMin;
        if constexpr (std::is_signed_v<TypeParam>)
        {
            bslt::CapSubFrom(v, TestFixture::kOne);
            EXPECT_EQ(v, TestFixture::kMin); // Saturated
        }
        else
        {
            // Unsigned 0 - 1 -> Max
            v = TestFixture::kZero;
            bslt::CapSubFrom(v, TestFixture::kOne);
            EXPECT_EQ(v, TestFixture::kMax);
        }
    }

    TYPED_TEST(SaturatedArithmeticTest, CapMulBy)
    {
        TypeParam v = TestFixture::kMax;
        bslt::CapMulBy(v, TestFixture::kTwo);
        EXPECT_EQ(v, TestFixture::kMax);

        v = TestFixture::kTwo;
        bslt::CapMulBy(v, TestFixture::kTwo);
        EXPECT_EQ(v, static_cast<TypeParam>(4));
    }

    TYPED_TEST(SaturatedArithmeticTest, CapDivBy)
    {
        auto v = static_cast<TypeParam>(10);
        bslt::CapDivBy(v, static_cast<TypeParam>(2));
        EXPECT_EQ(v, static_cast<TypeParam>(5));

        if constexpr (std::is_signed_v<TypeParam>)
        {
            v = TestFixture::kMin;
            bslt::CapDivBy(v, static_cast<TypeParam>(-1));
            EXPECT_EQ(v, TestFixture::kMax);
        }
    }
} // namespace bslt::test
