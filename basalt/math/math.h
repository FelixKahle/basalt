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

#ifndef BASALT_MATH_MATH_H_
#define BASALT_MATH_MATH_H_

#include <type_traits>
#include <cmath>
#include <limits>
#include <concepts>
#include "basalt/base/config.h"
#include "basalt/type_traits/type_traits.h"

namespace bslt
{
    /// @brief Computes the absolute value of an arithmetic type.
    ///
    /// This function returns the absolute value of the given arithmetic type.
    ///
    /// @tparam T The type of the value to compute the absolute value for.
    /// @param x The value for which to compute the absolute value.
    ///
    /// @note The introduction of this function is to provide a consistent way to compute absolute values
    /// for all arithmetic types, including both signed and unsigned integers, as well as floating-point types.
    /// The standard library provides `std::abs` for integers and `std::fabs` for floating-point types,
    /// but this function unifies the behavior across all arithmetic types.
    ///
    /// @return The absolute value of the input.
    template <typename T>
        requires std::is_arithmetic_v<T>
    [[nodiscard]] constexpr BASALT_FORCE_INLINE T Abs(const T x) noexcept
    {
        if constexpr (std::is_unsigned_v<T>)
        {
            // Unsigned types are always non-negative, so we can return them directly.
            // This will result in a zero cost operation for unsigned types,
            // and any major compiler will optimize this away.
            return x;
        }
        else if constexpr (std::is_floating_point_v<T>)
        {
            // Use std::fabs for floating-point types.
            // Overloads exist for both float and double types.
            // https://en.cppreference.com/w/cpp/numeric/math/fabs
            return std::fabs(x);
        }
        else
        {
            // For signed integral types, we check if the value is negative,
            // and if so, return its negation.
            return x < T{0} ? -x : x;
        }
    }

    /// @brief Checks if two arithmetic values are approximately equal.
    ///
    /// This function compares two arithmetic values and determines if they are
    /// approximately equal.
    ///
    /// @note
    /// For **floating-point** types, this function performs a relative error
    /// comparison. The `epsilon` parameter defines the maximum allowed relative
    /// difference: `Abs(left - right) <= (std::max(Abs(left), Abs(right)) * epsilon)`.
    /// An absolute tolerance is also used for comparisons near zero.
    ///
    /// For **integer** types, this function performs an absolute tolerance check:
    /// `Abs(left - right) <= epsilon`. The default epsilon for integers is 0,
    /// requiring an exact match.
    ///
    /// @tparam LeftType The type of the left-hand side value.
    /// @tparam RightType The type of the right-hand side value (default is LeftType).
    /// @tparam EpsilonType The type of the epsilon value (default is the common type).
    /// @param left The left-hand side value to compare.
    /// @param right The right-hand side value to compare.
    /// @param epsilon The tolerance to use for the comparison.
    ///
    /// @return \c true if the values are approximately equal, \c false otherwise.
    template <typename LeftType, typename RightType = LeftType, typename EpsilonType = std::common_type_t<LeftType, RightType>>
        requires std::is_arithmetic_v<LeftType> && std::is_arithmetic_v<RightType> && std::is_arithmetic_v<EpsilonType> && std::convertible_to<EpsilonType, std::common_type_t<LeftType, RightType>>
    [[nodiscard]] constexpr BASALT_FORCE_INLINE bool AlmostEqual(const LeftType left, const RightType right, const EpsilonType epsilon = std::numeric_limits<std::common_type_t<LeftType, RightType>>::epsilon()) noexcept
    {
        using CommonType = std::common_type_t<LeftType, RightType>;

        if constexpr (std::is_floating_point_v<CommonType>)
        {
            [[unlikely]] if (std::isinf(static_cast<CommonType>(left)) || std::isinf(static_cast<CommonType>(right)))
            {
                return left == right;
            }

            const CommonType diff = bslt::Abs<CommonType>(static_cast<CommonType>(left) - static_cast<CommonType>(right));
            const auto common_epsilon = static_cast<CommonType>(epsilon);

            // A minimal absolute tolerance to handle comparisons near zero
            const CommonType absolute_tolerance = std::numeric_limits<CommonType>::min();

            // Check if the difference is within the absolute tolerance (handles near-zero)
            if (diff <= absolute_tolerance)
            {
                return true;
            }

            // Otherwise, check the relative tolerance
            const CommonType absLeft = bslt::Abs(static_cast<CommonType>(left));
            const CommonType absRight = bslt::Abs(static_cast<CommonType>(right));
            const CommonType largest = (absLeft > absRight) ? absLeft : absRight;

            // The relative error check
            return diff <= (largest * common_epsilon);
        }
        else
        {
            using UnsignedCommon = std::make_unsigned_t<CommonType>;

            const auto u_epsilon = static_cast<UnsignedCommon>(epsilon);
            const UnsignedCommon diff = (left > right) ? (static_cast<UnsignedCommon>(left) - static_cast<UnsignedCommon>(right)) : (static_cast<UnsignedCommon>(right) - static_cast<UnsignedCommon>(left));
            return diff <= u_epsilon;
        }
    }

    /// @brief Checks if the given value is a power of two.
    ///
    /// This function checks whether the specified value is a power of two.
    ///
    /// @note Negative values are *NOT* considered powers of two, and zero is also *NOT* a power of two.
    ///
    /// @tparam T The type of the value. Must be an integral type.
    /// @param value The value to check.
    ///
    /// @return True if the value is a power of two, false otherwise.
    template <typename T>
        requires std::integral<T>
    [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IsPowerOfTwo(const T value) noexcept
    {
        return value > 0 && (value & (value - 1)) == 0;
    }
} // namespace bslt

#endif // BASALT_MATH_MATH_H_
