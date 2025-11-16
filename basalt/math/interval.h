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

#ifndef BASALT_MATH_INTERVAL_H_
#define BASALT_MATH_INTERVAL_H_

#include <algorithm>
#include <optional>
#include "basalt/base/config.h"
#include "basalt/type_traits/type_traits.h"

namespace bslt
{
    /// @brief Represents a closed-open interval [start, end).
    ///
    /// This class represents an interval where the start value is inclusive
    /// and the end value is exclusive.
    ///
    /// @tparam T The arithmetic type of the interval's endpoints.
    template <typename T>
        requires std::is_arithmetic_v<T>
    class ClosedOpenInterval
    {
    public:
        /// @brief Type alias for the value type of the interval.
        ///
        /// This type alias is used to refer to the type of the interval's endpoints.
        using ValueType = T;

        /// @brief Constructs an interval with the given start and end values.
        ///
        /// This constructor initializes the interval with the specified start and end values.
        ///
        /// @note The constructor ensures that the start value is always less than or equal to the end value.
        ///
        /// @param start_inclusive The inclusive start of the interval.
        /// @param end_exclusive The exclusive end of the interval.
        constexpr BASALT_FORCE_INLINE ClosedOpenInterval(const T start_inclusive, const T end_exclusive) noexcept
            : start_inclusive_(std::min<T>(start_inclusive, end_exclusive)),
              end_exclusive_(std::max<T>(start_inclusive, end_exclusive))
        {
        }

        /// @brief Gets the inclusive start of the interval.
        ///
        /// This method returns the inclusive start of the interval.
        ///
        /// @return The inclusive start of the interval.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T GetStart() const noexcept
        {
            return start_inclusive_;
        }

        /// @brief Gets the exclusive end of the interval.
        ///
        /// This method returns the exclusive end of the interval.
        ///
        /// @return The exclusive end of the interval.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T GetEnd() const noexcept
        {
            return end_exclusive_;
        }

        /// @brief Computes the midpoint of the interval.
        ///
        /// This method calculates the midpoint of the interval, which is the average of the start and end values.
        ///
        /// @tparam ReturnTimeType The type of the return value. Defaults to the type of the interval's endpoints.
        ///
        /// @return The midpoint of the interval as an arithmetic type.
        template <typename ReturnTimeType = T>
            requires std::is_arithmetic_v<ReturnTimeType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE ReturnTimeType Midpoint() const noexcept
        {
            if constexpr (std::is_floating_point_v<ReturnTimeType>)
            {
                return (static_cast<ReturnTimeType>(start_inclusive_) +
                    static_cast<ReturnTimeType>(end_exclusive_)) / ReturnTimeType{2};
            }

            return static_cast<ReturnTimeType>(start_inclusive_) +
                static_cast<ReturnTimeType>(end_exclusive_ - start_inclusive_) / ReturnTimeType{2};
        }

        /// @brief Checks if the interval is empty.
        ///
        /// This method checks if the interval has no length, which occurs when the start and end values are equal.
        ///
        /// @return \c true if the interval is empty, \c false otherwise.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IsEmpty() const noexcept
        {
            return start_inclusive_ == end_exclusive_;
        }

        /// @brief Gets the length of the interval.
        ///
        /// This method calculates the length of the interval, which is the difference between the end and start values.
        ///
        /// @return The length of the interval as an arithmetic type.
        [[nodiscard]] constexpr T Length() const noexcept
        {
            return end_exclusive_ - start_inclusive_;
        }

        /// @brief Checks if the interval contains the given value.
        ///
        /// This method checks if the specified value is within the interval, including the start but excluding the end.
        ///
        /// @tparam OtherType The type of the value to check. Must be convertible to the interval's type.
        /// @param value The value to check for containment in the interval.
        ///
        /// @return \c true if the value is within the interval, \c false otherwise.
        template <typename OtherType>
            requires std::convertible_to<OtherType, T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Contains(OtherType value) const noexcept
        {
            return value >= start_inclusive_ && value < end_exclusive_;
        }

        /// @brief Checks if the interval contains another interval.
        ///
        /// This method checks if the current interval fully contains another interval,
        /// meaning the start of the other interval is greater than or equal to the start of this interval,
        /// and the end of the other interval is less than or equal to the end of this interval.
        ///
        /// @tparam OtherType The type of the other interval's endpoints. Must be an arithmetic type.
        /// @param other The other interval to check for containment.
        ///
        /// @return \c true if the current interval contains the other interval, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool ContainsInterval(
            const ClosedOpenInterval<OtherType> other) const noexcept
        {
            return other.start_inclusive_ >= start_inclusive_
                && other.end_exclusive_ <= end_exclusive_;
        }

        /// @brief Checks if the current interval intersects with another interval.
        ///
        /// This method checks if the current interval overlaps with another interval,
        /// meaning there is at least one point that is contained in both intervals.
        ///
        /// @tparam OtherType The type of the other interval's endpoints. Must be an arithmetic type.
        /// @param other The other interval to check for intersection.
        ///
        /// @return \c true if the intervals intersect, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Intersects(const ClosedOpenInterval<OtherType> other) const noexcept
        {
            return std::max(start_inclusive_, static_cast<T>(other.GetStart())) <
                std::min(end_exclusive_, static_cast<T>(other.GetEnd()));
        }

        /// @brief Computes the intersection of the current interval with another interval.
        ///
        /// This method calculates the overlapping part of the current interval and another interval.
        /// If there is no intersection, it returns an empty optional.
        ///
        /// @tparam OtherType The type of the other interval's endpoints. Must be an arithmetic type.
        /// @tparam ReturnTimeType The type of the return value. Defaults to the type of the interval's endpoints.
        /// @param other The other interval to compute the intersection with.
        ///
        /// @return An optional containing the intersection interval if it exists,
        /// or an empty optional if there is no intersection.
        template <typename OtherType, typename ReturnTimeType = T>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<ClosedOpenInterval<ReturnTimeType>> Intersection(
            const ClosedOpenInterval<OtherType> other) const noexcept
        {
            const ReturnTimeType start = std::max(static_cast<ReturnTimeType>(start_inclusive_),
                                                  static_cast<ReturnTimeType>(other.GetStart()));
            const ReturnTimeType end = std::min(static_cast<ReturnTimeType>(end_exclusive_),
                                                static_cast<ReturnTimeType>(other.GetEnd()));

            if (start >= end)
            {
                return std::nullopt;
            }

            return ClosedOpenInterval<ReturnTimeType>(start, end);
        }

        /// @brief Clamps the current interval to fit within another interval.
        ///
        /// This method restricts the current interval to the boundaries defined by another interval.
        /// If the current interval does not overlap with the boundary interval, it returns an empty optional.
        ///
        /// @tparam OtherType The type of the boundary interval's endpoints. Must be an arithmetic type.
        ///
        /// @return An optional \c std::optional containing the clamped interval if it exists,
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<ClosedOpenInterval<T>> Clamp(
            const ClosedOpenInterval<OtherType> boundary) const noexcept
        {
            const T clamped_start = std::max(start_inclusive_, static_cast<T>(boundary.GetStart()));
            const T clamped_end = std::min(end_exclusive_, static_cast<T>(boundary.GetEnd()));

            if (clamped_start >= clamped_end)
            {
                return std::nullopt;
            }

            return ClosedOpenInterval(clamped_start, clamped_end);
        }

        /// @brief Compares two intervals for equality.
        ///
        /// This method checks if the start and end values of two intervals are equal.
        ///
        /// @tparam OtherType The type of the other interval's endpoints. Must be an arithmetic type.
        /// @param lhs The left-hand side interval to compare.
        /// @param rhs The other interval to compare with.
        ///
        /// @return \c true if the intervals are equal, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<OtherType>
        friend constexpr BASALT_FORCE_INLINE bool operator==(const ClosedOpenInterval lhs,
                                                                 const ClosedOpenInterval<OtherType> rhs) noexcept
        {
            return lhs.GetStart() == rhs.GetStart() && lhs.GetEnd() == rhs.GetEnd();
        }

        /// @brief Compares two intervals for inequality.
        ///
        /// This method checks if two intervals are not equal by negating the result of the equality operator.
        ///
        /// @tparam OtherType The type of the other interval's endpoints. Must be an arithmetic type.
        /// @param lhs The left-hand side interval to compare.
        /// @param rhs The other interval to compare with.
        ///
        /// @return \c true if the intervals are not equal, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<OtherType>
        friend constexpr BASALT_FORCE_INLINE bool operator!=(const ClosedOpenInterval lhs,
                                                                 const ClosedOpenInterval<OtherType> rhs) noexcept
        {
            return !(lhs == rhs);
        }

        /// @brief Absl hash function for the \c Interval class.
        ///
        /// This function computes a hash value for the interval by combining the hashes of its start and end values.
        ///
        /// @tparam H The hash type used by Absl.
        /// @param h The initial hash value.
        /// @param interval The interval for which to compute the hash.
        ///
        /// @return The combined hash value.
        template <typename H>
            requires AbslHasher<H>
        friend constexpr BASALT_FORCE_INLINE H AbslHashValue(H h, const ClosedOpenInterval interval) noexcept
        {
            return H::combine(std::move(h), interval.GetStart(), interval.GetEnd());
        }

        /// @brief Formats the interval as a string using Absl's formatting.
        ///
        /// This function formats the interval in the form
        /// "[start, end)" where "start" is inclusive and "end" is exclusive.
        ///
        /// @tparam Sink The type of the sink used for formatting.
        /// @param sink The sink to which the formatted string will be written.
        /// @param interval The interval to format.
        template <typename Sink>
            requires AbslStringifySink<Sink>
        friend BASALT_FORCE_INLINE void AbslStringify(Sink& sink, const ClosedOpenInterval interval) noexcept
        {
            absl::Format(&sink, "[%v, %v)", interval.GetStart(), interval.GetEnd());
        }

    private:
        T start_inclusive_;
        T end_exclusive_;
    };

    /// @brief Represents an open-closed interval (start, end].
    ///
    /// This class represents an interval where the start value is exclusive
    /// and the end value is inclusive.
    ///
    /// @tparam T The arithmetic type of the interval's endpoints.
    template <typename T>
        requires std::is_arithmetic_v<T>
    class OpenClosedInterval
    {
    public:
        using ValueType = T;

        /// @brief Constructs an interval with the given start and end values.
        ///
        /// @note The constructor ensures that the start value is always less than or equal to the end value.
        ///
        /// @param start_exclusive The exclusive start of the interval.
        /// @param end_inclusive The inclusive end of the interval.
        constexpr BASALT_FORCE_INLINE OpenClosedInterval(const T start_exclusive, const T end_inclusive) noexcept
            : start_exclusive_(std::min<T>(start_exclusive, end_inclusive)),
              end_inclusive_(std::max<T>(start_exclusive, end_inclusive))
        {
        }

        /// @brief Gets the exclusive start of the interval.
        /// @return The exclusive start of the interval.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T GetStart() const noexcept
        {
            return start_exclusive_;
        }

        /// @brief Gets the inclusive end of the interval.
        /// @return The inclusive end of the interval.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T GetEnd() const noexcept
        {
            return end_inclusive_;
        }

        /// @brief Computes the midpoint of the interval.
        /// @tparam ReturnTimeType The type of the return value. Defaults to T.
        /// @return The midpoint of the interval.
        template <typename ReturnTimeType = T>
            requires std::is_arithmetic_v<ReturnTimeType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE ReturnTimeType Midpoint() const noexcept
        {
            if constexpr (std::is_floating_point_v<ReturnTimeType>)
            {
                return (static_cast<ReturnTimeType>(start_exclusive_) +
                    static_cast<ReturnTimeType>(end_inclusive_)) / ReturnTimeType{2};
            }
            return static_cast<ReturnTimeType>(start_exclusive_) +
                static_cast<ReturnTimeType>(end_inclusive_ - start_exclusive_) / ReturnTimeType{2};
        }

        /// @brief Checks if the interval is empty (start >= end).
        /// @return \c true if the interval is empty, \c false otherwise.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IsEmpty() const noexcept
        {
            return start_exclusive_ == end_inclusive_;
        }

        /// @brief Gets the length of the interval (end - start).
        /// @return The length of the interval.
        [[nodiscard]] constexpr T Length() const noexcept
        {
            return end_inclusive_ - start_exclusive_;
        }

        /// @brief Checks if the interval contains the given value (value > start && value <= end).
        /// @tparam OtherType The type of the value to check.
        /// @param value The value to check.
        /// @return \c true if the value is within the interval, \c false otherwise.
        template <typename OtherType>
            requires std::convertible_to<OtherType, T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Contains(OtherType value) const noexcept
        {
            return value > start_exclusive_ && value <= end_inclusive_;
        }

        /// @brief Checks if this interval fully contains another interval.
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @return \c true if this interval contains the other, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool ContainsInterval(
            const OpenClosedInterval<OtherType> other) const noexcept
        {
            return other.GetStart() >= start_exclusive_
                && other.GetEnd() <= end_inclusive_;
        }

        /// @brief Checks if this interval intersects with another interval.
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @return \c true if the intervals intersect, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Intersects(const OpenClosedInterval<OtherType> other) const noexcept
        {
            return std::max(start_exclusive_, static_cast<T>(other.GetStart())) <
                std::min(end_inclusive_, static_cast<T>(other.GetEnd()));
        }

        /// @brief Computes the intersection of this interval with another.
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnTimeType The type of the resulting interval's endpoints.
        /// @param other The other interval.
        /// @return An optional containing the intersection, or nullopt if no intersection.
        template <typename OtherType, typename ReturnTimeType = T>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<OpenClosedInterval<ReturnTimeType>> Intersection(
            const OpenClosedInterval<OtherType> other) const noexcept
        {
            const ReturnTimeType start = std::max(static_cast<ReturnTimeType>(start_exclusive_),
                                                  static_cast<ReturnTimeType>(other.GetStart()));
            const ReturnTimeType end = std::min(static_cast<ReturnTimeType>(end_inclusive_),
                                                static_cast<ReturnTimeType>(other.GetEnd()));

            if (start >= end)
            {
                return std::nullopt;
            }

            return OpenClosedInterval<ReturnTimeType>(start, end);
        }

        /// @brief Clamps this interval to fit within a boundary interval.
        /// @tparam OtherType The type of the boundary interval's endpoints.
        /// @param boundary The boundary interval.
        /// @return An optional containing the clamped interval, or nullopt if no overlap.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<OpenClosedInterval<T>> Clamp(
            const OpenClosedInterval<OtherType> boundary) const noexcept
        {
            const T clamped_start = std::max(start_exclusive_, static_cast<T>(boundary.GetStart()));
            const T clamped_end = std::min(end_inclusive_, static_cast<T>(boundary.GetEnd()));

            if (clamped_start >= clamped_end)
            {
                return std::nullopt;
            }

            return OpenClosedInterval(clamped_start, clamped_end);
        }

        /// @brief Compares two intervals for equality.
        template <typename OtherType>
            requires std::is_arithmetic_v<OtherType>
        friend constexpr BASALT_FORCE_INLINE bool operator==(const OpenClosedInterval lhs,
                                                                 const OpenClosedInterval<OtherType> rhs) noexcept
        {
            return lhs.GetStart() == rhs.GetStart() && lhs.GetEnd() == rhs.GetEnd();
        }

        /// @brief Compares two intervals for inequality.
        template <typename OtherType>
            requires std::is_arithmetic_v<OtherType>
        friend constexpr BASALT_FORCE_INLINE bool operator!=(const OpenClosedInterval lhs,
                                                                 const OpenClosedInterval<OtherType> rhs) noexcept
        {
            return !(lhs == rhs);
        }

        /// @brief Absl hash function.
        template <typename H>
            requires AbslHasher<H>
        friend constexpr BASALT_FORCE_INLINE H AbslHashValue(H h, const OpenClosedInterval interval) noexcept
        {
            return H::combine(std::move(h), interval.GetStart(), interval.GetEnd());
        }

        /// @brief Absl string formatting function. Formats as (start, end].
        template <typename Sink>
            requires AbslStringifySink<Sink>
        friend BASALT_FORCE_INLINE void AbslStringify(Sink& sink, const OpenClosedInterval interval) noexcept
        {
            absl::Format(&sink, "(%v, %v]", interval.GetStart(), interval.GetEnd());
        }

    private:
        T start_exclusive_;
        T end_inclusive_;
    };

    /// @brief Represents a closed interval [start, end].
    ///
    /// This class represents an interval where both the start and end values
    /// are inclusive.
    ///
    /// @tparam T The arithmetic type of the interval's endpoints.
    template <typename T>
        requires std::is_arithmetic_v<T>
    class ClosedInterval
    {
    public:
        using ValueType = T;

        /// @brief Constructs an interval with the given start and end values.
        ///
        /// @note The constructor ensures that the start value is always less than or equal to the end value.
        ///
        /// @param start_inclusive The inclusive start of the interval.
        /// @param end_inclusive The inclusive end of the interval.
        constexpr BASALT_FORCE_INLINE ClosedInterval(const T start_inclusive, const T end_inclusive) noexcept
            : start_inclusive_(std::min<T>(start_inclusive, end_inclusive)),
              end_inclusive_(std::max<T>(start_inclusive, end_inclusive))
        {
        }

        /// @brief Gets the inclusive start of the interval.
        /// @return The inclusive start of the interval.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T GetStart() const noexcept
        {
            return start_inclusive_;
        }

        /// @brief Gets the inclusive end of the interval.
        /// @return The inclusive end of the interval.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T GetEnd() const noexcept
        {
            return end_inclusive_;
        }

        /// @brief Computes the midpoint of the interval.
        /// @tparam ReturnTimeType The type of the return value. Defaults to T.
        /// @return The midpoint of the interval.
        template <typename ReturnTimeType = T>
            requires std::is_arithmetic_v<ReturnTimeType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE ReturnTimeType Midpoint() const noexcept
        {
            if constexpr (std::is_floating_point_v<ReturnTimeType>)
            {
                return (static_cast<ReturnTimeType>(start_inclusive_) +
                    static_cast<ReturnTimeType>(end_inclusive_)) / ReturnTimeType{2};
            }
            return static_cast<ReturnTimeType>(start_inclusive_) +
                static_cast<ReturnTimeType>(end_inclusive_ - start_inclusive_) / ReturnTimeType{2};
        }

        /// @brief Checks if the interval is empty (start > end).
        /// @note Given the constructor invariant, this will always return false.
        /// @return \c true if the interval is empty, \c false otherwise.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IsEmpty() const noexcept
        {
            return start_inclusive_ > end_inclusive_;
        }

        /// @brief Gets the length of the interval (end - start).
        /// @return The length of the interval.
        [[nodiscard]] constexpr T Length() const noexcept
        {
            return end_inclusive_ - start_inclusive_;
        }

        /// @brief Checks if the interval contains the given value (value >= start && value <= end).
        /// @tparam OtherType The type of the value to check.
        /// @param value The value to check.
        /// @return \c true if the value is within the interval, \c false otherwise.
        template <typename OtherType>
            requires std::convertible_to<OtherType, T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Contains(OtherType value) const noexcept
        {
            return value >= start_inclusive_ && value <= end_inclusive_;
        }

        /// @brief Checks if this interval fully contains another interval.
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @return \c true if this interval contains the other, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool ContainsInterval(
            const ClosedInterval<OtherType> other) const noexcept
        {
            return other.GetStart() >= start_inclusive_
                && other.GetEnd() <= end_inclusive_;
        }

        /// @brief Checks if this interval intersects with another interval.
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @return \c true if the intervals intersect, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Intersects(const ClosedInterval<OtherType> other) const noexcept
        {
            return std::max(start_inclusive_, static_cast<T>(other.GetStart())) <=
                std::min(end_inclusive_, static_cast<T>(other.GetEnd()));
        }

        /// @brief Computes the intersection of this interval with another.
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnTimeType The type of the resulting interval's endpoints.
        /// @param other The other interval.
        /// @return An optional containing the intersection, or nullopt if no intersection.
        template <typename OtherType, typename ReturnTimeType = T>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<ClosedInterval<ReturnTimeType>> Intersection(
            const ClosedInterval<OtherType> other) const noexcept
        {
            const ReturnTimeType start = std::max(static_cast<ReturnTimeType>(start_inclusive_),
                                                  static_cast<ReturnTimeType>(other.GetStart()));
            const ReturnTimeType end = std::min(static_cast<ReturnTimeType>(end_inclusive_),
                                                static_cast<ReturnTimeType>(other.GetEnd()));

            if (start > end)
            {
                return std::nullopt;
            }

            return ClosedInterval<ReturnTimeType>(start, end);
        }

        /// @brief Clamps this interval to fit within a boundary interval.
        /// @tparam OtherType The type of the boundary interval's endpoints.
        /// @param boundary The boundary interval.
        /// @return An optional containing the clamped interval, or nullopt if no overlap.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<ClosedInterval<T>> Clamp(
            const ClosedInterval<OtherType> boundary) const noexcept
        {
            const T clamped_start = std::max(start_inclusive_, static_cast<T>(boundary.GetStart()));
            const T clamped_end = std::min(end_inclusive_, static_cast<T>(boundary.GetEnd()));

            if (clamped_start > clamped_end)
            {
                return std::nullopt;
            }

            return ClosedInterval(clamped_start, clamped_end);
        }

        /// @brief Compares two intervals for equality.
        template <typename OtherType>
            requires std::is_arithmetic_v<OtherType>
        friend constexpr BASALT_FORCE_INLINE bool operator==(const ClosedInterval lhs,
                                                                 const ClosedInterval<OtherType> rhs) noexcept
        {
            return lhs.GetStart() == rhs.GetStart() && lhs.GetEnd() == rhs.GetEnd();
        }

        /// @brief Compares two intervals for inequality.
        template <typename OtherType>
            requires std::is_arithmetic_v<OtherType>
        friend constexpr BASALT_FORCE_INLINE bool operator!=(const ClosedInterval lhs,
                                                                 const ClosedInterval<OtherType> rhs) noexcept
        {
            return !(lhs == rhs);
        }

        /// @brief Absl hash function.
        template <typename H>
            requires AbslHasher<H>
        friend constexpr BASALT_FORCE_INLINE H AbslHashValue(H h, const ClosedInterval interval) noexcept
        {
            return H::combine(std::move(h), interval.GetStart(), interval.GetEnd());
        }

        /// @brief Absl string formatting function. Formats as [start, end].
        template <typename Sink>
            requires AbslStringifySink<Sink>
        friend BASALT_FORCE_INLINE void AbslStringify(Sink& sink, const ClosedInterval interval) noexcept
        {
            absl::Format(&sink, "[%v, %v]", interval.GetStart(), interval.GetEnd());
        }

    private:
        T start_inclusive_;
        T end_inclusive_;
    };

    /// @brief Represents an open interval (start, end).
    ///
    /// This class represents an interval where both the start and end values
    /// are exclusive.
    ///
    /// @tparam T The arithmetic type of the interval's endpoints.
    template <typename T>
        requires std::is_arithmetic_v<T>
    class OpenInterval
    {
    public:
        using ValueType = T;

        /// @brief Constructs an interval with the given start and end values.
        ///
        /// @note The constructor ensures that the start value is always less than or equal to the end value.
        ///
        /// @param start_exclusive The exclusive start of the interval.
        /// @param end_exclusive The exclusive end of the interval.
        constexpr BASALT_FORCE_INLINE OpenInterval(const T start_exclusive, const T end_exclusive) noexcept
            : start_exclusive_(std::min<T>(start_exclusive, end_exclusive)),
              end_exclusive_(std::max<T>(start_exclusive, end_exclusive))
        {
        }

        /// @brief Gets the exclusive start of the interval.
        /// @return The exclusive start of the interval.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T GetStart() const noexcept
        {
            return start_exclusive_;
        }

        /// @brief Gets the exclusive end of the interval.
        /// @return The exclusive end of the interval.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T GetEnd() const noexcept
        {
            return end_exclusive_;
        }

        /// @brief Computes the midpoint of the interval.
        /// @tparam ReturnTimeType The type of the return value. Defaults to T.
        /// @return The midpoint of the interval.
        template <typename ReturnTimeType = T>
            requires std::is_arithmetic_v<ReturnTimeType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE ReturnTimeType Midpoint() const noexcept
        {
            if constexpr (std::is_floating_point_v<ReturnTimeType>)
            {
                return (static_cast<ReturnTimeType>(start_exclusive_) +
                    static_cast<ReturnTimeType>(end_exclusive_)) / ReturnTimeType{2};
            }
            return static_cast<ReturnTimeType>(start_exclusive_) +
                static_cast<ReturnTimeType>(end_exclusive_ - start_exclusive_) / ReturnTimeType{2};
        }

        /// @brief Checks if the interval is empty (start >= end).
        /// @return \c true if the interval is empty, \c false otherwise.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IsEmpty() const noexcept
        {
            return start_exclusive_ == end_exclusive_;
        }

        /// @brief Gets the length of the interval (end - start).
        /// @return The length of the interval.
        [[nodiscard]] constexpr T Length() const noexcept
        {
            return end_exclusive_ - start_exclusive_;
        }

        /// @brief Checks if the interval contains the given value (value > start && value < end).
        /// @tparam OtherType The type of the value to check.
        /// @param value The value to check.
        /// @return \c true if the value is within the interval, \c false otherwise.
        template <typename OtherType>
            requires std::convertible_to<OtherType, T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Contains(OtherType value) const noexcept
        {
            return value > start_exclusive_ && value < end_exclusive_;
        }

        /// @brief Checks if this interval fully contains another interval.
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @return \c true if this interval contains the other, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool ContainsInterval(
            const OpenInterval<OtherType> other) const noexcept
        {
            return other.GetStart() >= start_exclusive_
                && other.GetEnd() <= end_exclusive_;
        }

        /// @brief Checks if this interval intersects with another interval.
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @return \c true if the intervals intersect, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Intersects(const OpenInterval<OtherType> other) const noexcept
        {
            return std::max(start_exclusive_, static_cast<T>(other.GetStart())) <
                std::min(end_exclusive_, static_cast<T>(other.GetEnd()));
        }

        /// @brief Computes the intersection of this interval with another.
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnTimeType The type of the resulting interval's endpoints.
        /// @param other The other interval.
        /// @return An optional containing the intersection, or nullopt if no intersection.
        template <typename OtherType, typename ReturnTimeType = T>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<OpenInterval<ReturnTimeType>> Intersection(
            const OpenInterval<OtherType> other) const noexcept
        {
            const ReturnTimeType start = std::max(static_cast<ReturnTimeType>(start_exclusive_),
                                                  static_cast<ReturnTimeType>(other.GetStart()));
            const ReturnTimeType end = std::min(static_cast<ReturnTimeType>(end_exclusive_),
                                                static_cast<ReturnTimeType>(other.GetEnd()));

            if (start >= end)
            {
                return std::nullopt;
            }

            return OpenInterval<ReturnTimeType>(start, end);
        }

        /// @brief Clamps this interval to fit within a boundary interval.
        /// @tparam OtherType The type of the boundary interval's endpoints.
        /// @param boundary The boundary interval.
        /// @return An optional containing the clamped interval, or nullopt if no overlap.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<OpenInterval<T>> Clamp(
            const OpenInterval<OtherType> boundary) const noexcept
        {
            const T clamped_start = std::max(start_exclusive_, static_cast<T>(boundary.GetStart()));
            const T clamped_end = std::min(end_exclusive_, static_cast<T>(boundary.GetEnd()));

            if (clamped_start >= clamped_end)
            {
                return std::nullopt;
            }

            return OpenInterval(clamped_start, clamped_end);
        }

        /// @brief Compares two intervals for equality.
        template <typename OtherType>
            requires std::is_arithmetic_v<OtherType>
        friend constexpr BASALT_FORCE_INLINE bool operator==(const OpenInterval lhs,
                                                                 const OpenInterval<OtherType> rhs) noexcept
        {
            return lhs.GetStart() == rhs.GetStart() && lhs.GetEnd() == rhs.GetEnd();
        }

        /// @brief Compares two intervals for inequality.
        template <typename OtherType>
            requires std::is_arithmetic_v<OtherType>
        friend constexpr BASALT_FORCE_INLINE bool operator!=(const OpenInterval lhs,
                                                                 const OpenInterval<OtherType> rhs) noexcept
        {
            return !(lhs == rhs);
        }

        /// @brief Absl hash function.
        template <typename H>
            requires AbslHasher<H>
        friend constexpr BASALT_FORCE_INLINE H AbslHashValue(H h, const OpenInterval interval) noexcept
        {
            return H::combine(std::move(h), interval.GetStart(), interval.GetEnd());
        }

        /// @brief Absl string formatting function. Formats as (start, end).
        template <typename Sink>
            requires AbslStringifySink<Sink>
        friend BASALT_FORCE_INLINE void AbslStringify(Sink& sink, const OpenInterval interval) noexcept
        {
            absl::Format(&sink, "(%v, %v)", interval.GetStart(), interval.GetEnd());
        }

    private:
        T start_exclusive_;
        T end_exclusive_;
    };
}

#endif // BASALT_MATH_INTERVAL_H_
