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

#ifndef BASALT_UTILS_SATURATED_ARITHMETIC_H_
#define BASALT_UTILS_SATURATED_ARITHMETIC_H_

// This will be deprecated when C++26 is widely available, as it introduces
// std::sub_sat etc. As the time of writing (2025), C++26 is not yet widely supported.

#include <limits>
#include <type_traits>
#include <utility>
#if defined(_MSC_VER)
#include <intrin.h>
#endif
#include "absl/log/check.h"
#include "basalt/base/config.h"

namespace bslt::saturated_arithmetic
{
    /// @brief Performs addition ignoring overflow (wrap-around behavior).
    ///
    /// This function casts the operands to their unsigned counterparts to perform
    /// the addition. In C++, signed integer overflow is Undefined Behavior (UB),
    /// while unsigned integer overflow is defined to wrap around (modulo arithmetic).
    /// This ensures safe execution even when the mathematical result exceeds the type's range.
    ///
    /// @tparam T The integral type of the operands.
    /// @param x The first operand.
    /// @param y The second operand.
    /// @result The result of (x + y) wrapped around the limits of type T.
    template <typename T>
        requires std::is_integral_v<T>
    constexpr BASALT_FORCE_INLINE T WrappingAdd(const T x, const T y) noexcept
    {
        using U = std::make_unsigned_t<T>;
        return static_cast<T>(static_cast<U>(x) + static_cast<U>(y));
    }

    /// @brief Performs subtraction ignoring overflow (wrap-around behavior).
    ///
    /// Similar to WrappingAdd, this function casts operands to unsigned types to perform
    /// subtraction safely. This prevents Undefined Behavior associated with signed
    /// integer underflow/overflow in C++.
    ///
    /// @tparam T The integral type of the operands.
    /// @param x The minuend.
    /// @param y The subtrahend.
    /// @result The result of (x - y) wrapped around the limits of type T.
    template <typename T>
        requires std::is_integral_v<T>
    constexpr BASALT_FORCE_INLINE T WrappingSub(const T x, const T y) noexcept
    {
        using U = std::make_unsigned_t<T>;
        return static_cast<T>(static_cast<U>(x) - static_cast<U>(y));
    }

    /// @brief Returns the minimum or maximum value of type T based on the input sign.
    ///
    /// This helper is used to determine the saturation target when an overflow is detected.
    /// If T is unsigned, it always returns the maximum value.
    /// If T is signed, it returns T::min() if x is negative, and T::max() otherwise.
    /// Modern compilers optimize this to a conditional move (CMOV) or bitwise mask.
    ///
    /// @tparam T The integral type.
    /// @param x The value whose sign determines the limit direction.
    /// @result The saturation limit (Min or Max) corresponding to the direction of x.
    template <typename T>
        requires std::is_integral_v<T>
    constexpr BASALT_FORCE_INLINE T SaturateToLimitBasedOnSign(const T x) noexcept
    {
        if constexpr (std::is_unsigned_v<T>)
        {
            return std::numeric_limits<T>::max();
        }
        else
        {
            return (x < 0) ? std::numeric_limits<T>::min() : std::numeric_limits<T>::max();
        }
    }

    /// @brief Casts a value from a source type to a destination type with saturation.
    ///
    /// If the source value is outside the representable range of the destination type,
    /// the result is clamped to the nearest representable value (Min or Max of Dst).
    /// This function uses C++20 `std::cmp_*` functions to safely handle comparisons
    /// between signed and unsigned types without implicit conversion pitfalls.
    ///
    /// @tparam Dst The destination integral type.
    /// @tparam Src The source integral type.
    /// @param value The value to cast.
    /// @result The value cast to Dst, saturated to [Dst::min(), Dst::max()] if out of bounds.
    template <typename Dst, typename Src>
        requires std::is_integral_v<Dst> && std::is_integral_v<Src>
    constexpr BASALT_FORCE_INLINE Dst SaturatedCast(const Src value) noexcept
    {
        if (std::cmp_greater(value, std::numeric_limits<Dst>::max()))
        {
            return std::numeric_limits<Dst>::max();
        }
        if (std::cmp_less(value, std::numeric_limits<Dst>::min()))
        {
            return std::numeric_limits<Dst>::min();
        }
        return static_cast<Dst>(value);
    }

    /// @brief Checks if a value is at the minimum or maximum limit of its type.
    ///
    /// For floating point types, this checks against `lowest()` (most negative)
    /// and `max()` (most positive). For integral types, it checks `min()` and `max()`.
    ///
    /// @tparam T The arithmetic type.
    /// @param x The value to check.
    /// @result True if x is the minimum or maximum representable value, false otherwise.
    template <typename T>
        requires std::is_arithmetic_v<T>
    constexpr BASALT_FORCE_INLINE bool IsAtNumericLimit(const T x) noexcept
    {
        if constexpr (std::is_floating_point_v<T>)
        {
            return x == std::numeric_limits<T>::lowest() || x == std::numeric_limits<T>::max();
        }
        else
        {
            return x == std::numeric_limits<T>::min() || x == std::numeric_limits<T>::max();
        }
    }

    /// @brief Checks if the addition of two values would result in an overflow.
    ///
    /// This function detects both positive overflow and negative underflow (for signed types).
    /// It uses compiler builtins where available (GCC/Clang) for optimal performance.
    /// On other platforms, it uses a branchless bitwise check for signed integers and
    /// a wrap-around check for unsigned integers.
    ///
    /// @tparam T The integral type.
    /// @param x The first operand.
    /// @param y The second operand.
    /// @result True if (x + y) cannot be represented in type T, false otherwise.
    template <typename T>
        requires std::is_integral_v<T>
    constexpr BASALT_FORCE_INLINE bool WillAdditionOverflow(const T x, const T y) noexcept
    {
#if defined(__GNUC__) || defined(__clang__)
        T dummy;
        return __builtin_add_overflow(x, y, &dummy);
#else
        // MSVC / Generic Fallback
        if constexpr (std::is_unsigned_v<T>)
        {
            // Unsigned overflow: Result wraps around and becomes smaller than operand.
            return WrappingAdd(x, y) < x;
        }
        else
        {
            // Signed overflow: Operands have same sign, but result has different sign.
            // (Pos + Pos = Neg) or (Neg + Neg = Pos).
            const T sum = WrappingAdd(x, y);
            // This bitwise check is branchless and extremely fast.
            return ((x ^ sum) & (y ^ sum)) < 0;
        }
#endif
    }

    /// @brief Checks if the subtraction of two values would result in an overflow.
    ///
    /// This function detects overflow/underflow for the operation (x - y).
    /// It uses compiler builtins where available. On fallback paths, it performs
    /// checks based on the wrap-around properties of the subtraction.
    ///
    /// @tparam T The integral type.
    /// @param x The minuend.
    /// @param y The subtrahend.
    /// @result True if (x - y) cannot be represented in type T, false otherwise.
    template <typename T>
        requires std::is_integral_v<T>
    constexpr BASALT_FORCE_INLINE bool WillSubtractionOverflow(const T x, const T y) noexcept
    {
#if defined(__GNUC__) || defined(__clang__)
        T dummy;
        return __builtin_sub_overflow(x, y, &dummy);
#else
        // MSVC / Generic Fallback
        if constexpr (std::is_unsigned_v<T>)
        {
            return x < y;
        }
        else
        {
            // Signed overflow: x = diff + y.
            // Overflow if diff and y have same sign, but x has different sign.
            const T diff = WrappingSub(x, y);
            return ((diff ^ x) & (y ^ x)) < 0;
        }
#endif
    }

    /// @brief Returns the saturated negation of a value.
    ///
    /// For most values, this returns -v. However, for the minimum representable
    /// value (e.g., INT_MIN), negation causes overflow. In this case, the function
    /// returns the maximum representable value (INT_MAX).
    ///
    /// @tparam T The signed integral type.
    /// @param v The value to negate.
    /// @result The saturated negation of v.
    template <typename T>
        requires std::is_signed_v<T>
    constexpr BASALT_FORCE_INLINE T SaturatedNegate(const T v) noexcept
    {
        return (v == std::numeric_limits<T>::min()) ? std::numeric_limits<T>::max() : -v;
    }

    /// @brief Returns the saturated absolute value of a value.
    ///
    /// For the minimum representable value (e.g., INT_MIN), the absolute value
    /// cannot be represented. This function saturates that case to INT_MAX.
    ///
    /// @tparam T The signed integral type.
    /// @param v The value to compute the absolute of.
    /// @result The saturated absolute value of v.
    template <typename T>
        requires std::is_signed_v<T>
    constexpr BASALT_FORCE_INLINE T SaturatedAbs(const T v) noexcept
    {
        return (v == std::numeric_limits<T>::min()) ? std::numeric_limits<T>::max() : (v < 0 ? -v : v);
    }

    /// @brief Performs saturated addition.
    ///
    /// Computes (x + y). If the result overflows the type T, it returns
    /// T::max() (for positive overflow) or T::min() (for negative underflow).
    ///
    /// @tparam T The integral type.
    /// @param x The first operand.
    /// @param y The second operand.
    /// @result The sum of x and y, clamped to the representable range of T.
    template <typename T>
        requires std::is_integral_v<T>
    constexpr BASALT_FORCE_INLINE T SaturatedAdd(const T x, const T y) noexcept
    {
#if defined(__GNUC__) || defined(__clang__)
        T res;
        if (__builtin_add_overflow(x, y, &res))
        {
            return SaturateToLimitBasedOnSign(x);
        }
        return res;
#else
        const T res = WrappingAdd(x, y);
        bool overflow;

        if constexpr (std::is_unsigned_v<T>)
        {
            overflow = res < x;
        }
        else
        {
            overflow = ((x ^ res) & (y ^ res)) < 0;
        }

        if (overflow)
        {
            return SaturateToLimitBasedOnSign(x);
        }
        return res;
#endif
    }

    /// @brief Performs saturated subtraction.
    ///
    /// Computes (x - y). If the result overflows the type T, it returns
    /// T::max() or T::min() depending on the direction of the overflow.
    ///
    /// @tparam T The integral type.
    /// @param x The minuend.
    /// @param y The subtrahend.
    /// @result The difference of x and y, clamped to the representable range of T.
    template <typename T>
        requires std::is_integral_v<T>
    constexpr BASALT_FORCE_INLINE T SaturatedSub(const T x, const T y) noexcept
    {
#if defined(__GNUC__) || defined(__clang__)
        T res;
        if (__builtin_sub_overflow(x, y, &res))
        {
            return SaturateToLimitBasedOnSign(x);
        }
        return res;
#else
        const T res = WrappingSub(x, y);
        bool overflow;

        if constexpr (std::is_unsigned_v<T>)
        {
            overflow = x < y;
        }
        else
        {
            // Overflow if res and y have same sign, but x has different sign
            overflow = ((res ^ x) & (y ^ x)) < 0;
        }

        if (overflow)
        {
            return SaturateToLimitBasedOnSign(x);
        }
        return res;
#endif
    }

    /// @brief Performs saturated multiplication.
    ///
    /// Computes (x * y). If the product exceeds the range of T, it returns
    /// T::max() or T::min(). This function uses platform-specific intrinsics
    /// (like `_mul128` on MSVC x64) or compiler builtins (`__builtin_mul_overflow`)
    /// to achieve maximum performance without expensive division checks where possible.
    ///
    /// @tparam T The integral type.
    /// @param x The first operand.
    /// @param y The second operand.
    /// @result The product of x and y, clamped to the representable range of T.
    template <typename T>
        requires std::is_integral_v<T>
    constexpr BASALT_FORCE_INLINE T SaturatedMul(const T x, const T y) noexcept
    {
#if defined(__GNUC__) || defined(__clang__)
        T res;
        if (__builtin_mul_overflow(x, y, &res))
        {
            if constexpr (std::is_unsigned_v<T>)
            {
                return std::numeric_limits<T>::max();
            }
            else
            {
                return SaturateToLimitBasedOnSign(static_cast<T>(x ^ y));
            }
        }
        return res;
#elif defined(_MSC_VER) && defined(_M_X64)
        if constexpr (sizeof(T) == 8)
        {
            if constexpr (std::is_unsigned_v<T>)
            {
                unsigned __int64 high;
                unsigned __int64 low = _umul128(x, y, &high);
                // If high bits are set, we overflowed.
                if (high != 0)
                {
                    return std::numeric_limits<T>::max();
                }
                return static_cast<T>(low);
            }
            else
            {
                __int64 high;
                __int64 low = _mul128(x, y, &high);
                // In a valid signed 64-bit result, the high 64 bits must be
                // the sign extension of the low 64 bits.
                // i.e., high must be 0 (if positive) or -1 (if negative).
                // We check this by shifting the low part arithmetic right by 63.
                if (high != (low >> 63))
                {
                    return SaturateToLimitBasedOnSign(static_cast<T>(x ^ y));
                }
                return static_cast<T>(low);
            }
        }
#endif
        if constexpr (sizeof(T) < sizeof(int64_t))
        {
            using WideT = std::conditional_t<std::is_signed_v<T>, int64_t, uint64_t>;
            const WideT wide_res = static_cast<WideT>(x) * static_cast<WideT>(y);
            if (wide_res > static_cast<WideT>(std::numeric_limits<T>::max()))
            {
                return std::numeric_limits<T>::max();
            }
            if constexpr (std::is_signed_v<T>)
            {
                if (wide_res < static_cast<WideT>(std::numeric_limits<T>::min()))
                {
                    return std::numeric_limits<T>::min();
                }
            }
            return static_cast<T>(wide_res);
        }
        else
        {
            if (x == 0 || y == 0)
            {
                return T{0};
            }

            if constexpr (std::is_unsigned_v<T>)
            {
                if (x > std::numeric_limits<T>::max() / y)
                {
                    return std::numeric_limits<T>::max();
                }
            }
            else
            {
                if (x > 0 && y > 0 && x > std::numeric_limits<T>::max() / y)
                {
                    return std::numeric_limits<T>::max();
                }
                if (x > 0 && y < 0 && y < std::numeric_limits<T>::min() / x)
                {
                    return std::numeric_limits<T>::min();
                }
                if (x < 0 && y > 0 && x < std::numeric_limits<T>::min() / y)
                {
                    return std::numeric_limits<T>::min();
                }
                if (x < 0 && y < 0 && x < std::numeric_limits<T>::max() / y)
                {
                    return std::numeric_limits<T>::max();
                }
            }
            return x * y;
        }
    }

    /// @brief Performs saturated division.
    ///
    /// Standard integer division truncates, but it can overflow/crash in one specific case:
    /// `INT_MIN / -1` results in `INT_MAX + 1` which cannot be represented.
    /// This function checks for that case and saturates to `INT_MAX` instead of crashing (SIGFPE).
    ///
    /// @tparam T The integral type.
    /// @param x The dividend.
    /// @param y The divisor.
    /// @result The quotient of x / y, clamped if the result overflows.
    template <typename T>
        requires std::is_integral_v<T>
    constexpr BASALT_FORCE_INLINE T SaturatedDiv(const T x, const T y) noexcept
    {
        // Overflow in division happens only for Signed Min / -1.
        // This causes a SIGFPE (Floating Point Exception) on many CPUs (x86),
        // effectively crashing the program.
        if constexpr (std::is_signed_v<T>)
        {
            if (x == std::numeric_limits<T>::min() && y == static_cast<T>(-1))
            {
                return std::numeric_limits<T>::max();
            }
        }
        return x / y;
    }

    /// @brief Adds y to x in place, using saturated arithmetic.
    ///
    /// @tparam T The integral type.
    /// @param x The variable to accumulate into.
    /// @param y The value to add.
    template <typename T>
        requires std::is_integral_v<T>
    constexpr BASALT_FORCE_INLINE void SaturatedAddAssign(T& x, const T y) noexcept
    {
        x = SaturatedAdd(x, y);
    }

    /// @brief Subtracts y from x in place, using saturated arithmetic.
    ///
    /// @tparam T The integral type.
    /// @param x The variable to subtract from.
    /// @param y The value to subtract.
    template <typename T>
        requires std::is_integral_v<T>
    constexpr BASALT_FORCE_INLINE void SaturatedSubtractAssign(T& x, const T y) noexcept
    {
        x = SaturatedSub(x, y);
    }

    /// @brief Multiplies x by y in place, using saturated arithmetic.
    ///
    /// @tparam T The integral type.
    /// @param x The variable to multiply.
    /// @param y The factor to multiply by.
    template <typename T>
        requires std::is_integral_v<T>
    constexpr BASALT_FORCE_INLINE void SaturatedMultiplyAssign(T& x, const T y) noexcept
    {
        x = SaturatedMul(x, y);
    }

    /// @brief Divides x by y in place, using saturated arithmetic.
    ///
    /// @tparam T The integral type.
    /// @param x The variable to divide.
    /// @param y The divisor.
    template <typename T>
        requires std::is_integral_v<T>
    constexpr BASALT_FORCE_INLINE void SaturatedDivideAssign(T& x, const T y) noexcept
    {
        x = SaturatedDiv(x, y);
    }
}

#endif // BASALT_UTILS_SATURATED_ARITHMETIC_H_
