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

namespace test
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

    TYPED_TEST(SaturatedArithmeticTest, IsAtNumericLimit)
    {
        EXPECT_TRUE(bslt::saturated_arithmetic::IsAtNumericLimit(TestFixture::kMin));
        EXPECT_TRUE(bslt::saturated_arithmetic::IsAtNumericLimit(TestFixture::kMax));

        // For unsigned types, kMin is 0, so IsAtNumericLimit(0) is true.
        // For signed types, 0 is neither min nor max.
        if constexpr (std::is_signed_v<TypeParam>)
        {
            EXPECT_FALSE(bslt::saturated_arithmetic::IsAtNumericLimit(TestFixture::kZero));
        }
        else
        {
            EXPECT_TRUE(bslt::saturated_arithmetic::IsAtNumericLimit(TestFixture::kZero));
        }

        // 1 is usually not min or max (unless type is 1-bit, not applicable here)
        EXPECT_FALSE(bslt::saturated_arithmetic::IsAtNumericLimit(TestFixture::kOne));
    }

    TYPED_TEST(SaturatedArithmeticTest, WillAdditionOverflow)
    {
        // No overflow
        EXPECT_FALSE(bslt::saturated_arithmetic::WillAdditionOverflow(TestFixture::kOne, TestFixture::kOne));
        EXPECT_FALSE(bslt::saturated_arithmetic::WillAdditionOverflow(TestFixture::kZero, TestFixture::kMax));

        // Overflow
        EXPECT_TRUE(bslt::saturated_arithmetic::WillAdditionOverflow(TestFixture::kMax, TestFixture::kOne));

        if constexpr (std::is_signed_v<TypeParam>)
        {
            // Underflow (Negative Overflow)
            EXPECT_TRUE(
                bslt::saturated_arithmetic::WillAdditionOverflow(TestFixture::kMin, static_cast<TypeParam>(-1)));
        }
    }

    TYPED_TEST(SaturatedArithmeticTest, WillSubtractionOverflow)
    {
        // No overflow
        EXPECT_FALSE(bslt::saturated_arithmetic::WillSubtractionOverflow(TestFixture::kMax, TestFixture::kOne));
        EXPECT_FALSE(bslt::saturated_arithmetic::WillSubtractionOverflow(TestFixture::kOne, TestFixture::kOne));

        if constexpr (std::is_unsigned_v<TypeParam>)
        {
            // Unsigned wrap-around (0 - 1) is detected as overflow
            EXPECT_TRUE(bslt::saturated_arithmetic::WillSubtractionOverflow(TestFixture::kZero, TestFixture::kOne));
        }
        else
        {
            // Signed: Max - (-1) = Overflow
            EXPECT_TRUE(
                bslt::saturated_arithmetic::WillSubtractionOverflow(TestFixture::kMax, static_cast<TypeParam>(-1)));
            // Signed: Min - 1 = Underflow
            EXPECT_TRUE(bslt::saturated_arithmetic::WillSubtractionOverflow(TestFixture::kMin, TestFixture::kOne));
        }
    }

    // -----------------------------------------------------------------------
    // Saturated Casting
    // -----------------------------------------------------------------------

    TYPED_TEST(SaturatedArithmeticTest, SaturatedCast)
    {
        // 1. Same type cast (Identity)
        EXPECT_EQ(bslt::saturated_arithmetic::SaturatedCast<TypeParam>(TestFixture::kMax), TestFixture::kMax);
        EXPECT_EQ(bslt::saturated_arithmetic::SaturatedCast<TypeParam>(TestFixture::kMin), TestFixture::kMin);

        // 2. Larger (int64_t) to TypeParam (Narrowing)
        int64_t huge = std::numeric_limits<int64_t>::max();
        if constexpr (sizeof(TypeParam) < 8 || (sizeof(TypeParam) == 8 && std::is_signed_v<TypeParam>))
        {
            // int64_max fits in uint64, fits in int64.
            // If TypeParam is smaller, or TypeParam is int64 (identity), this block runs.
            if constexpr (sizeof(TypeParam) < 8)
            {
                EXPECT_EQ(bslt::saturated_arithmetic::SaturatedCast<TypeParam>(huge), TestFixture::kMax);
            }
        }

        int64_t tiny = std::numeric_limits<int64_t>::min();
        if constexpr (std::is_unsigned_v<TypeParam>)
        {
            // Negative -> 0 (Min of unsigned)
            EXPECT_EQ(bslt::saturated_arithmetic::SaturatedCast<TypeParam>(tiny), TestFixture::kMin);
            EXPECT_EQ(bslt::saturated_arithmetic::SaturatedCast<TypeParam>(-1), TestFixture::kMin);
        }
        else
        {
            // Signed narrowing
            if constexpr (sizeof(TypeParam) < 8)
            {
                EXPECT_EQ(bslt::saturated_arithmetic::SaturatedCast<TypeParam>(tiny), TestFixture::kMin);
            }
        }

        // 3. Unsigned -> Signed conversion
        // e.g., uint8(255) -> int8(127)
        if constexpr (std::is_signed_v<TypeParam>)
        {
            using UnsignedVer = std::make_unsigned_t<TypeParam>;
            auto u_max = std::numeric_limits<UnsignedVer>::max();
            EXPECT_EQ(bslt::saturated_arithmetic::SaturatedCast<TypeParam>(u_max), TestFixture::kMax);
        }
    }

    // -----------------------------------------------------------------------
    // Unary Operations (Signed Only)
    // -----------------------------------------------------------------------

    TYPED_TEST(SaturatedArithmeticTest, SaturatedNegate)
    {
        if constexpr (std::is_signed_v<TypeParam>)
        {
            // Normal
            EXPECT_EQ(bslt::saturated_arithmetic::SaturatedNegate(TestFixture::kOne), static_cast<TypeParam>(-1));
            // Saturation: -Min -> Max
            EXPECT_EQ(bslt::saturated_arithmetic::SaturatedNegate(TestFixture::kMin), TestFixture::kMax);
        }
    }

    TYPED_TEST(SaturatedArithmeticTest, SaturatedAbs)
    {
        if constexpr (std::is_signed_v<TypeParam>)
        {
            // Normal
            EXPECT_EQ(bslt::saturated_arithmetic::SaturatedAbs(static_cast<TypeParam>(-1)), TestFixture::kOne);
            // Saturation: Abs(Min) -> Max
            EXPECT_EQ(bslt::saturated_arithmetic::SaturatedAbs(TestFixture::kMin), TestFixture::kMax);
        }
    }

    // -----------------------------------------------------------------------
    // Binary Operations: Add, Sub, Mul
    // -----------------------------------------------------------------------

    TYPED_TEST(SaturatedArithmeticTest, SaturatedAdd)
    {
        // Normal
        EXPECT_EQ(bslt::saturated_arithmetic::SaturatedAdd(TestFixture::kOne, TestFixture::kOne), TestFixture::kTwo);

        // Saturation High
        EXPECT_EQ(bslt::saturated_arithmetic::SaturatedAdd(TestFixture::kMax, TestFixture::kOne), TestFixture::kMax);

        // Saturation Low (Signed)
        if constexpr (std::is_signed_v<TypeParam>)
        {
            EXPECT_EQ(bslt::saturated_arithmetic::SaturatedAdd(TestFixture::kMin, static_cast<TypeParam>(-1)),
                      TestFixture::kMin);
        }
    }

    TYPED_TEST(SaturatedArithmeticTest, SaturatedSub)
    {
        // Normal
        EXPECT_EQ(bslt::saturated_arithmetic::SaturatedSub(TestFixture::kTwo, TestFixture::kOne), TestFixture::kOne);

        if constexpr (std::is_unsigned_v<TypeParam>)
        {
            // 0 - 1 wraps to Max. Overflow detected.
            // SaturateToLimitBasedOnSign(0) returns Max (for unsigned).
            // Result is Max.
            EXPECT_EQ(bslt::saturated_arithmetic::SaturatedSub(TestFixture::kZero, TestFixture::kOne),
                      TestFixture::kMax);
        }
        else
        {
            // Signed: Min - 1 -> Min
            EXPECT_EQ(bslt::saturated_arithmetic::SaturatedSub(TestFixture::kMin, TestFixture::kOne),
                      TestFixture::kMin);
            // Signed: Max - (-1) -> Max
            EXPECT_EQ(bslt::saturated_arithmetic::SaturatedSub(TestFixture::kMax, static_cast<TypeParam>(-1)),
                      TestFixture::kMax);
        }
    }

    TYPED_TEST(SaturatedArithmeticTest, SaturatedMul)
    {
        // Normal
        EXPECT_EQ(bslt::saturated_arithmetic::SaturatedMul(TestFixture::kTwo, TestFixture::kTwo),
                  static_cast<TypeParam>(4));

        // Saturation
        EXPECT_EQ(bslt::saturated_arithmetic::SaturatedMul(TestFixture::kMax, TestFixture::kTwo), TestFixture::kMax);

        if constexpr (std::is_signed_v<TypeParam>)
        {
            // Min * 2 -> Min
            EXPECT_EQ(bslt::saturated_arithmetic::SaturatedMul(TestFixture::kMin, TestFixture::kTwo),
                      TestFixture::kMin);
            // Min * -1 -> Max
            EXPECT_EQ(bslt::saturated_arithmetic::SaturatedMul(TestFixture::kMin, static_cast<TypeParam>(-1)),
                      TestFixture::kMax);
        }
    }

    // -----------------------------------------------------------------------
    // Division
    // -----------------------------------------------------------------------

    TYPED_TEST(SaturatedArithmeticTest, SaturatedDiv)
    {
        // Normal
        EXPECT_EQ(bslt::saturated_arithmetic::SaturatedDiv(static_cast<TypeParam>(10), static_cast<TypeParam>(2)),
                  static_cast<TypeParam>(5));

        if constexpr (std::is_signed_v<TypeParam>)
        {
            // The SIGFPE Case: Min / -1 -> Max
            EXPECT_EQ(bslt::saturated_arithmetic::SaturatedDiv(TestFixture::kMin, static_cast<TypeParam>(-1)),
                      TestFixture::kMax);

            // Other cases
            EXPECT_EQ(bslt::saturated_arithmetic::SaturatedDiv(TestFixture::kMin, TestFixture::kOne),
                      TestFixture::kMin);
        }
    }

    // -----------------------------------------------------------------------
    // In-Place Operations
    // -----------------------------------------------------------------------

    TYPED_TEST(SaturatedArithmeticTest, SaturatedAddAssign)
    {
        TypeParam v = TestFixture::kMax;
        bslt::saturated_arithmetic::SaturatedAddAssign(v, TestFixture::kOne);
        EXPECT_EQ(v, TestFixture::kMax); // Saturated

        v = TestFixture::kOne;
        bslt::saturated_arithmetic::SaturatedAddAssign(v, TestFixture::kOne);
        EXPECT_EQ(v, TestFixture::kTwo);
    }

    TYPED_TEST(SaturatedArithmeticTest, SaturatedSubtractAssign)
    {
        TypeParam v = TestFixture::kMin;
        if constexpr (std::is_signed_v<TypeParam>)
        {
            bslt::saturated_arithmetic::SaturatedSubtractAssign(v, TestFixture::kOne);
            EXPECT_EQ(v, TestFixture::kMin); // Saturated
        }
        else
        {
            // Unsigned 0 - 1 -> Max
            v = TestFixture::kZero;
            bslt::saturated_arithmetic::SaturatedSubtractAssign(v, TestFixture::kOne);
            EXPECT_EQ(v, TestFixture::kMax);
        }
    }

    TYPED_TEST(SaturatedArithmeticTest, SaturatedMultiplyAssign)
    {
        TypeParam v = TestFixture::kMax;
        bslt::saturated_arithmetic::SaturatedMultiplyAssign(v, TestFixture::kTwo);
        EXPECT_EQ(v, TestFixture::kMax);

        v = TestFixture::kTwo;
        bslt::saturated_arithmetic::SaturatedMultiplyAssign(v, TestFixture::kTwo);
        EXPECT_EQ(v, static_cast<TypeParam>(4));
    }

    TYPED_TEST(SaturatedArithmeticTest, SaturatedDivideAssign)
    {
        auto v = static_cast<TypeParam>(10);
        bslt::saturated_arithmetic::SaturatedDivideAssign(v, static_cast<TypeParam>(2));
        EXPECT_EQ(v, static_cast<TypeParam>(5));

        if constexpr (std::is_signed_v<TypeParam>)
        {
            v = TestFixture::kMin;
            bslt::saturated_arithmetic::SaturatedDivideAssign(v, static_cast<TypeParam>(-1));
            EXPECT_EQ(v, TestFixture::kMax);
        }
    }
} // namespace test
