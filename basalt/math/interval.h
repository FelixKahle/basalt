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
#include <concepts>
#include <functional>
#include <type_traits>
#include "absl/log/check.h"
#include "absl/strings/str_format.h"
#include "absl/hash/hash.h"
#include "basalt/base/config.h"
#include "basalt/type_traits/type_traits.h"

namespace bslt
{
    /// @brief A concept that defines the requirements for an Interval type.
    ///
    /// This concept specifies the necessary member functions and properties
    /// that a type must implement to be considered an Interval.
    ///
    /// @tparam T The type to be checked for Interval properties.
    template <typename T>
    concept Interval = requires(const T& interval, const T& other, typename T::ValueType v)
    {
        typename T::ValueType;
        requires std::is_arithmetic_v<typename T::ValueType>;

        // The start value of the interval
        { interval.GetStart() } -> std::convertible_to<typename T::ValueType>;

        // The end value of the interval
        { interval.GetEnd() } -> std::convertible_to<typename T::ValueType>;

        // Check if the interval is empty
        { interval.IsEmpty() } -> std::same_as<bool>;
        { interval.IsEmpty(v) } -> std::same_as<bool>;

        // The length of the interval
        { interval.Length() } -> std::convertible_to<typename T::ValueType>;

        // The midpoint of the interval
        { interval.Midpoint() } -> std::convertible_to<typename T::ValueType>;

        // Check if a value is contained within the interval
        { interval.Contains(v) } -> std::same_as<bool>;
        { interval.Contains(v, v) } -> std::same_as<bool>;

        // Check if another interval is contained within this interval
        { interval.ContainsInterval(other) } -> std::same_as<bool>;
        { interval.ContainsInterval(other, v) } -> std::same_as<bool>;

        // Check if this interval intersects with another interval.
        { interval.Intersects(other) } -> std::same_as<bool>;
        { interval.Intersects(other, v) } -> std::same_as<bool>;

        // Check if this interval intersects or is adjacent to another interval.
        { interval.IntersectsOrAdjacent(other) } -> std::same_as<bool>;
        { interval.IntersectsOrAdjacent(other, v) } -> std::same_as<bool>;

        // Check if this interval is adjacent to another interval.
        { interval.Adjacent(other) } -> std::same_as<bool>;
        { interval.Adjacent(other, v) } -> std::same_as<bool>;

        // Calculate the distance to another interval.
        { interval.DistanceTo(other) } -> std::convertible_to<typename T::ValueType>;

        // Get the intersection of this interval with another interval.
        { interval.Intersection(other) } -> std::same_as<std::optional<T>>;

        // Clamp this interval to another interval.
        { interval.Clamp(other) } -> std::same_as<std::optional<T>>;

        // Merge this interval with another interval.
        { interval.Merge(other) } -> std::same_as<std::optional<T>>;
        { interval.Merge(other, v) } -> std::same_as<std::optional<T>>;

        // Combine this interval with another interval.
        { interval.Combine(other) } -> std::same_as<T>;
    };

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
        using ValueType = T;

        /// @brief Default constructor creating an empty interval [0, 0).
        ///
        /// This constructor initializes both the start and end values to zero,
        /// resulting in an empty interval.
        constexpr BASALT_FORCE_INLINE ClosedOpenInterval() noexcept
            : start_inclusive_(T{0}),
              end_exclusive_(T{0})
        {
        }

        /// @brief Constructs an empty interval [0, 0).
        ///
        /// This static method returns an interval where both the start and end
        /// values are zero, representing an empty interval.
        static constexpr BASALT_FORCE_INLINE ClosedOpenInterval Empty() noexcept
        {
            return ClosedOpenInterval(T{0}, T{0});
        }

        /// @brief Constructs an interval with the given start and end values.
        ///
        /// This constructor initializes the interval with the specified start and end values.
        ///
        /// @param start_inclusive The inclusive start of the interval.
        /// @param end_exclusive The exclusive end of the interval.
        constexpr BASALT_FORCE_INLINE ClosedOpenInterval(const T start_inclusive, const T end_exclusive) noexcept
            : start_inclusive_(start_inclusive),
              end_exclusive_(end_exclusive)
        {
            DCHECK_LE(start_inclusive, end_exclusive);
        }

        /// @brief Gets the inclusive start of the interval.
        ///
        /// This method returns the lower bound of the interval, which is included in the set.
        ///
        /// @return The inclusive start value.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T GetStart() const noexcept
        {
            return start_inclusive_;
        }

        /// @brief Gets the exclusive end of the interval.
        ///
        /// This method returns the upper bound of the interval, which is excluded from the set.
        ///
        /// @return The exclusive end value.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T GetEnd() const noexcept
        {
            return end_exclusive_;
        }

        /// @brief Computes the midpoint of the interval.
        ///
        /// This method calculates the arithmetic mean of the start and end values.
        /// For floating point types, it returns the exact center. For integer types, it truncates towards zero.
        ///
        /// @tparam ReturnValueType The type of the returned midpoint. Defaults to T.
        ///
        /// @return The midpoint of the interval.
        template <typename ReturnValueType = T>
            requires std::is_arithmetic_v<ReturnValueType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE ReturnValueType Midpoint() const noexcept
        {
            if constexpr (std::is_floating_point_v<ReturnValueType>)
            {
                return (static_cast<ReturnValueType>(start_inclusive_)
                    + static_cast<ReturnValueType>(end_exclusive_)) / ReturnValueType{2};
            }
            return static_cast<ReturnValueType>(start_inclusive_) +
                static_cast<ReturnValueType>(end_exclusive_ - start_inclusive_) / ReturnValueType{2};
        }

        /// @brief Checks if the interval is empty.
        ///
        /// This method checks if the interval contains no points.
        /// For a closed-open interval [a, b), it is empty if a == b.
        ///
        /// @return \c true if the interval is empty, \c false otherwise.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IsEmpty() const noexcept
        {
            return start_inclusive_ == end_exclusive_;
        }

        /// @brief Checks if the interval is empty using a tolerance epsilon.
        ///
        /// This method checks if the length of the interval is less than or equal to the given epsilon.
        ///
        /// @param epsilon The tolerance value for checking emptiness.
        ///
        /// @return \c true if the interval length is <= epsilon, \c false otherwise.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IsEmpty(const T epsilon) const noexcept
        {
            return (end_exclusive_ - start_inclusive_) <= epsilon;
        }

        /// @brief Gets the length of the interval.
        ///
        /// This method calculates the difference between the end and start values.
        ///
        /// @return The length of the interval.
        [[nodiscard]] constexpr T Length() const noexcept
        {
            return end_exclusive_ - start_inclusive_;
        }

        /// @brief Checks if the interval contains the given value.
        ///
        /// This method checks if the value is within [start, end).
        ///
        /// @tparam OtherType The type of the value to check.
        /// @param value The value to check.
        ///
        /// @return \c true if the value is contained, \c false otherwise.
        template <typename OtherType>
            requires std::convertible_to<OtherType, T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Contains(OtherType value) const noexcept
        {
            return value >= start_inclusive_ && value < end_exclusive_;
        }

        /// @brief Checks if the interval contains the given value with tolerance.
        ///
        /// This method checks if the value is within [start - epsilon, end + epsilon).
        ///
        /// @tparam OtherType The type of the value to check.
        /// @param value The value to check.
        /// @param epsilon The tolerance buffer to expand the interval boundaries.
        ///
        /// @return \c true if the value is contained within the expanded interval, \c false otherwise.
        template <typename OtherType>
            requires std::convertible_to<OtherType, T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Contains(OtherType value, const T epsilon) const noexcept
        {
            return value >= (start_inclusive_ - epsilon) && value < (end_exclusive_ + epsilon);
        }

        /// @brief Checks if the interval fully contains another interval.
        ///
        /// This method checks if the other interval is a subset of this interval.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        ///
        /// @return \c true if this interval contains the other interval, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool ContainsInterval(
            const ClosedOpenInterval<OtherType> other) const noexcept
        {
            return other.GetStart() >= start_inclusive_ && other.GetEnd() <= end_exclusive_;
        }

        /// @brief Checks if the interval fully contains another interval with tolerance.
        ///
        /// This method checks if the other interval is a subset of this interval, expanded by epsilon.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @param epsilon The tolerance value used to expand this interval's boundaries.
        ///
        /// @return \c true if this interval (expanded) contains the other interval, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool ContainsInterval(const ClosedOpenInterval<OtherType> other,
                                                                          const T epsilon) const noexcept
        {
            return other.GetStart() >= (start_inclusive_ - epsilon) &&
                other.GetEnd() <= (end_exclusive_ + epsilon);
        }

        /// @brief Checks if the current interval intersects with another interval.
        ///
        /// This method determines if there is any overlap between the two intervals.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        ///
        /// @return \c true if the intervals intersect, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Intersects(
            const ClosedOpenInterval<OtherType> other) const noexcept
        {
            return std::max(start_inclusive_, static_cast<T>(other.GetStart())) <
                std::min(end_exclusive_, static_cast<T>(other.GetEnd()));
        }

        /// @brief Checks if the current interval intersects with another interval with tolerance.
        ///
        /// This method determines if there is any overlap between the two intervals, allowing for a fuzzy intersection.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @param epsilon The tolerance value.
        ///
        /// @return \c true if the intervals intersect within epsilon, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Intersects(const ClosedOpenInterval<OtherType> other,
                                                                    const T epsilon) const noexcept
        {
            return std::max(start_inclusive_, static_cast<T>(other.GetStart())) <
                std::min(end_exclusive_, static_cast<T>(other.GetEnd())) + epsilon;
        }

        /// @brief Checks if this interval is adjacent to another interval.
        ///
        /// Adjacency implies the intervals touch exactly at an endpoint but do not strictly overlap.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        ///
        /// @return \c true if the intervals are adjacent, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Adjacent(
            const ClosedOpenInterval<OtherType> other) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());
            return end_exclusive_ == other_start || start_inclusive_ == other_end;
        }

        /// @brief Checks if this interval is adjacent to another interval with tolerance.
        ///
        /// This checks if the endpoints of the two intervals are within epsilon of each other.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @param epsilon The tolerance value.
        ///
        /// @return \c true if the intervals are adjacent within epsilon, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Adjacent(const ClosedOpenInterval<OtherType> other,
                                                                  const T epsilon) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());

            // We use max-min here to safely handle unsigned types where abs() might differ
            auto diff1 = (end_exclusive_ > other_start)
                             ? (end_exclusive_ - other_start)
                             : (other_start - end_exclusive_);
            auto diff2 = (start_inclusive_ > other_end)
                             ? (start_inclusive_ - other_end)
                             : (other_end - start_inclusive_);

            return diff1 <= epsilon || diff2 <= epsilon;
        }

        /// @brief Checks if this interval intersects or touches another interval.
        ///
        /// This checks if the two intervals are connected (non-disjoint).
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        ///
        /// @return \c true if the intervals intersect or touch, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IntersectsOrAdjacent(
            const ClosedOpenInterval<OtherType> other) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());
            return end_exclusive_ >= other_start && other_end >= start_inclusive_;
        }

        /// @brief Checks if this interval intersects or touches another interval with tolerance.
        ///
        /// This checks if the two intervals are connected within an epsilon tolerance.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @param epsilon The tolerance value.
        ///
        /// @return \c true if the intervals intersect or are adjacent within epsilon, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IntersectsOrAdjacent(
            const ClosedOpenInterval<OtherType> other, const T epsilon) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());
            return end_exclusive_ >= (other_start - epsilon) && other_end >= (start_inclusive_ - epsilon);
        }

        /// @brief Calculate the distance to another interval.
        ///
        /// The distance is defined as the minimum distance between the two intervals.
        /// If the intervals intersect or touch, the distance is 0.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to calculate the distance to.
        ///
        /// @return The distance value.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T DistanceTo(
            const ClosedOpenInterval<OtherType> other) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());

            if (IntersectsOrAdjacent(other))
            {
                return T{0};
            }
            if (end_exclusive_ < other_start)
            {
                return other_start - end_exclusive_;
            }
            return start_inclusive_ - other_end;
        }

        /// @brief Merges this interval with another interval.
        ///
        /// Creates a new interval spanning the minimum start and maximum end of both intervals,
        /// but only if they intersect or are adjacent.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnValueType The value type of the resulting interval.
        /// @param other The other interval to merge with.
        ///
        /// @return An optional containing the merged interval, or std::nullopt if they are disjoint.
        template <typename OtherType, typename ReturnValueType = T>
            requires std::is_arithmetic_v<T> && std::is_arithmetic_v<OtherType> && std::is_arithmetic_v<ReturnValueType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<ClosedOpenInterval<ReturnValueType>> Merge(
            const ClosedOpenInterval<OtherType> other) const noexcept
        {
            if (!IntersectsOrAdjacent(other))
            {
                return std::nullopt;
            }
            const ReturnValueType start = std::min(static_cast<ReturnValueType>(start_inclusive_),
                                                   static_cast<ReturnValueType>(other.GetStart()));
            const ReturnValueType end = std::max(static_cast<ReturnValueType>(end_exclusive_),
                                                 static_cast<ReturnValueType>(other.GetEnd()));
            return ClosedOpenInterval<ReturnValueType>(start, end);
        }

        /// @brief Merges this interval with another interval using tolerance to bridge gaps.
        ///
        /// Creates a new interval spanning the minimum start and maximum end of both intervals,
        /// provided they are connected within the given epsilon.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnValueType The value type of the resulting interval.
        /// @param other The other interval.
        /// @param epsilon The tolerance value used to determine connectivity.
        ///
        /// @return An optional containing the merged interval, or std::nullopt if they are disjoint beyond epsilon.
        template <typename OtherType, typename ReturnValueType = T>
            requires std::is_arithmetic_v<T> && std::is_arithmetic_v<OtherType> && std::is_arithmetic_v<ReturnValueType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<ClosedOpenInterval<ReturnValueType>> Merge(
            const ClosedOpenInterval<OtherType> other, const T epsilon) const noexcept
        {
            if (!IntersectsOrAdjacent(other, epsilon))
            {
                return std::nullopt;
            }
            const ReturnValueType start = std::min(static_cast<ReturnValueType>(start_inclusive_),
                                                   static_cast<ReturnValueType>(other.GetStart()));
            const ReturnValueType end = std::max(static_cast<ReturnValueType>(end_exclusive_),
                                                 static_cast<ReturnValueType>(other.GetEnd()));
            return ClosedOpenInterval<ReturnValueType>(start, end);
        }

        /// @brief Combines this interval with another interval.
        ///
        /// Returns a new interval that encompasses both intervals (the bounding box),
        /// regardless of whether they intersect or not.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnValueType The value type of the resulting interval.
        /// @param other The other interval to combine with.
        ///
        /// @return A new interval representing the bounding range of both inputs.
        template <typename OtherType, typename ReturnValueType = T>
            requires std::is_arithmetic_v<T> && std::is_arithmetic_v<OtherType> && std::is_arithmetic_v<ReturnValueType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE ClosedOpenInterval<ReturnValueType> Combine(
            const ClosedOpenInterval<OtherType> other) const noexcept
        {
            const ReturnValueType start = std::min(static_cast<ReturnValueType>(start_inclusive_),
                                                   static_cast<ReturnValueType>(other.GetStart()));
            const ReturnValueType end = std::max(static_cast<ReturnValueType>(end_exclusive_),
                                                 static_cast<ReturnValueType>(other.GetEnd()));
            return ClosedOpenInterval<ReturnValueType>(start, end);
        }

        /// @brief Computes the intersection of the current interval with another interval.
        ///
        /// This method returns the interval representing the overlapping region.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnValueType The value type of the resulting interval.
        /// @param other The other interval to compute the intersection with.
        ///
        /// @return An optional containing the intersection interval, or std::nullopt if they do not intersect.
        template <typename OtherType, typename ReturnValueType = T>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<ClosedOpenInterval<ReturnValueType>> Intersection(
            const ClosedOpenInterval<OtherType> other) const noexcept
        {
            const ReturnValueType start = std::max(static_cast<ReturnValueType>(start_inclusive_),
                                                   static_cast<ReturnValueType>(other.GetStart()));
            const ReturnValueType end = std::min(static_cast<ReturnValueType>(end_exclusive_),
                                                 static_cast<ReturnValueType>(other.GetEnd()));
            if (start >= end)
            {
                return std::nullopt;
            }
            return ClosedOpenInterval<ReturnValueType>(start, end);
        }

        /// @brief Clamps the current interval to fit within another interval.
        ///
        /// This restricts the start and end of the current interval to not exceed the boundaries of the provided interval.
        ///
        /// @tparam OtherType The type of the boundary interval's endpoints.
        /// @param boundary The interval defining the valid range.
        ///
        /// @return An optional containing the clamped interval, or std::nullopt if the result would be empty.
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
        /// Checks if the start and end points are exactly equal.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param lhs The left-hand side interval.
        /// @param rhs The right-hand side interval.
        ///
        /// @return \c true if intervals are equal, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<OtherType>
        friend constexpr BASALT_FORCE_INLINE bool operator==(const ClosedOpenInterval lhs,
                                                             const ClosedOpenInterval<OtherType> rhs) noexcept
        {
            return lhs.GetStart() == rhs.GetStart() && lhs.GetEnd() == rhs.GetEnd();
        }

        /// @brief Compares two intervals for inequality.
        ///
        /// Checks if the start or end points differ.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param lhs The left-hand side interval.
        /// @param rhs The right-hand side interval.
        ///
        /// @return \c true if intervals are not equal, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<OtherType>
        friend constexpr BASALT_FORCE_INLINE bool operator!=(const ClosedOpenInterval lhs,
                                                             const ClosedOpenInterval<OtherType> rhs) noexcept
        {
            return !(lhs == rhs);
        }

        friend std::ostream& operator<<(std::ostream& os, const ClosedOpenInterval interval)
        {
            return os << "[" << interval.GetStart() << ", " << interval.GetEnd() << ")";
        }

        /// @brief Computes the hash value for the interval.
        ///
        /// This function is compatible with the Abseil hashing framework.
        ///
        /// @tparam H The hasher type.
        /// @param h The hasher state.
        /// @param interval The interval to hash.
        ///
        /// @return The updated hasher state.
        template <typename H>
            requires AbslHasher<H>
        friend constexpr BASALT_FORCE_INLINE H AbslHashValue(H h, const ClosedOpenInterval interval) noexcept
        {
            return H::combine(std::move(h), interval.GetStart(), interval.GetEnd());
        }

        /// @brief formats the interval for logging or debugging.
        ///
        /// Formats the interval as "[start, end)".
        ///
        /// @tparam Sink The sink type.
        /// @param sink The sink to write to.
        /// @param interval The interval to stringify.
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

        /// @brief Default constructor creating an empty interval (0, 0].
        ///
        /// This constructor initializes both the start and end values to zero,
        /// resulting in an empty interval.
        constexpr BASALT_FORCE_INLINE OpenClosedInterval() noexcept
            : start_exclusive_(T{0}),
              end_inclusive_(T{0})
        {
        }

        /// @brief Constructs an empty interval (0, 0].
        ///
        /// This static method returns an interval where both the start and end
        /// values are zero, representing an empty interval.
        static constexpr BASALT_FORCE_INLINE OpenClosedInterval Empty() noexcept
        {
            return OpenClosedInterval(T{0}, T{0});
        }

        /// @brief Constructs an interval with the given start and end values.
        ///
        /// This constructor initializes the interval with the specified start and end values.
        ///
        /// @param start_exclusive The exclusive start of the interval.
        /// @param end_inclusive The inclusive end of the interval.
        constexpr BASALT_FORCE_INLINE OpenClosedInterval(const T start_exclusive, const T end_inclusive) noexcept
            : start_exclusive_(start_exclusive),
              end_inclusive_(end_inclusive)
        {
            DCHECK_LE(start_exclusive, end_inclusive);
        }

        /// @brief Gets the exclusive start of the interval.
        ///
        /// This method returns the lower bound of the interval, which is excluded from the set.
        ///
        /// @return The exclusive start value.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T GetStart() const noexcept
        {
            return start_exclusive_;
        }

        /// @brief Gets the inclusive end of the interval.
        ///
        /// This method returns the upper bound of the interval, which is included in the set.
        ///
        /// @return The inclusive end value.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T GetEnd() const noexcept
        {
            return end_inclusive_;
        }

        /// @brief Computes the midpoint of the interval.
        ///
        /// This method calculates the arithmetic mean of the start and end values.
        /// For floating point types, it returns the exact center. For integer types, it truncates towards zero.
        ///
        /// @tparam ReturnValueType The type of the returned midpoint. Defaults to T.
        ///
        /// @return The midpoint of the interval.
        template <typename ReturnValueType = T>
            requires std::is_arithmetic_v<ReturnValueType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE ReturnValueType Midpoint() const noexcept
        {
            if constexpr (std::is_floating_point_v<ReturnValueType>)
            {
                return (static_cast<ReturnValueType>(start_exclusive_)
                    + static_cast<ReturnValueType>(end_inclusive_)) / ReturnValueType{2};
            }
            return static_cast<ReturnValueType>(start_exclusive_) +
                static_cast<ReturnValueType>(end_inclusive_ - start_exclusive_) / ReturnValueType{2};
        }

        /// @brief Checks if the interval is empty.
        ///
        /// This method checks if the interval contains no points.
        /// For an open-closed interval (a, b], it is empty if a >= b.
        ///
        /// @return \c true if the interval is empty, \c false otherwise.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IsEmpty() const noexcept
        {
            return start_exclusive_ >= end_inclusive_;
        }

        /// @brief Checks if the interval is empty using a tolerance epsilon.
        ///
        /// This method checks if the length of the interval is less than or equal to the given epsilon.
        ///
        /// @param epsilon The tolerance value for checking emptiness.
        ///
        /// @return \c true if the interval length is <= epsilon, \c false otherwise.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IsEmpty(const T epsilon) const noexcept
        {
            return (end_inclusive_ - start_exclusive_) <= epsilon;
        }

        /// @brief Gets the length of the interval.
        ///
        /// This method calculates the difference between the end and start values.
        ///
        /// @return The length of the interval.
        [[nodiscard]] constexpr T Length() const noexcept
        {
            return end_inclusive_ - start_exclusive_;
        }

        /// @brief Checks if the interval contains the given value.
        ///
        /// This method checks if the value is within (start, end].
        ///
        /// @tparam OtherType The type of the value to check.
        /// @param value The value to check.
        ///
        /// @return \c true if the value is contained, \c false otherwise.
        template <typename OtherType>
            requires std::convertible_to<OtherType, T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Contains(OtherType value) const noexcept
        {
            return value > start_exclusive_ && value <= end_inclusive_;
        }

        /// @brief Checks if the interval contains the given value with tolerance.
        ///
        /// This method checks if the value is within (start - epsilon, end + epsilon].
        ///
        /// @tparam OtherType The type of the value to check.
        /// @param value The value to check.
        /// @param epsilon The tolerance buffer to expand the interval boundaries.
        ///
        /// @return \c true if the value is contained within the expanded interval, \c false otherwise.
        template <typename OtherType>
            requires std::convertible_to<OtherType, T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Contains(OtherType value, const T epsilon) const noexcept
        {
            return value > (start_exclusive_ - epsilon) && value <= (end_inclusive_ + epsilon);
        }

        /// @brief Checks if the interval fully contains another interval.
        ///
        /// This method checks if the other interval is a subset of this interval.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        ///
        /// @return \c true if this interval contains the other interval, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool ContainsInterval(
            const OpenClosedInterval<OtherType> other) const noexcept
        {
            return other.GetStart() >= start_exclusive_ && other.GetEnd() <= end_inclusive_;
        }

        /// @brief Checks if the interval fully contains another interval with tolerance.
        ///
        /// This method checks if the other interval is a subset of this interval, expanded by epsilon.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @param epsilon The tolerance value used to expand this interval's boundaries.
        ///
        /// @return \c true if this interval (expanded) contains the other interval, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool ContainsInterval(const OpenClosedInterval<OtherType> other,
                                                                          const T epsilon) const noexcept
        {
            return other.GetStart() >= (start_exclusive_ - epsilon) &&
                other.GetEnd() <= (end_inclusive_ + epsilon);
        }

        /// @brief Checks if the current interval intersects with another interval.
        ///
        /// This method determines if there is any overlap between the two intervals.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        ///
        /// @return \c true if the intervals intersect, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Intersects(
            const OpenClosedInterval<OtherType> other) const noexcept
        {
            return std::max(start_exclusive_, static_cast<T>(other.GetStart())) <
                std::min(end_inclusive_, static_cast<T>(other.GetEnd()));
        }

        /// @brief Checks if the current interval intersects with another interval with tolerance.
        ///
        /// This method determines if there is any overlap between the two intervals, allowing for a fuzzy intersection.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @param epsilon The tolerance value.
        ///
        /// @return \c true if the intervals intersect within epsilon, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Intersects(const OpenClosedInterval<OtherType> other,
                                                                    const T epsilon) const noexcept
        {
            return std::max(start_exclusive_, static_cast<T>(other.GetStart())) <
                std::min(end_inclusive_, static_cast<T>(other.GetEnd())) + epsilon;
        }

        /// @brief Checks if this interval is adjacent to another interval.
        ///
        /// Adjacency implies the intervals touch exactly at an endpoint but do not strictly overlap.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        ///
        /// @return \c true if the intervals are adjacent, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Adjacent(
            const OpenClosedInterval<OtherType> other) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());
            return end_inclusive_ == other_start || start_exclusive_ == other_end;
        }

        /// @brief Checks if this interval is adjacent to another interval with tolerance.
        ///
        /// This checks if the endpoints of the two intervals are within epsilon of each other.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @param epsilon The tolerance value.
        ///
        /// @return \c true if the intervals are adjacent within epsilon, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Adjacent(const OpenClosedInterval<OtherType> other,
                                                                  const T epsilon) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());

            auto diff1 = (end_inclusive_ > other_start)
                             ? (end_inclusive_ - other_start)
                             : (other_start - end_inclusive_);
            auto diff2 = (start_exclusive_ > other_end)
                             ? (start_exclusive_ - other_end)
                             : (other_end - start_exclusive_);

            return diff1 <= epsilon || diff2 <= epsilon;
        }

        /// @brief Checks if this interval intersects or touches another interval.
        ///
        /// This checks if the two intervals are connected (non-disjoint).
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        ///
        /// @return \c true if the intervals intersect or touch, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IntersectsOrAdjacent(
            const OpenClosedInterval<OtherType> other) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());
            return end_inclusive_ >= other_start && other_end >= start_exclusive_;
        }

        /// @brief Checks if this interval intersects or touches another interval with tolerance.
        ///
        /// This checks if the two intervals are connected within an epsilon tolerance.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @param epsilon The tolerance value.
        ///
        /// @return \c true if the intervals intersect or are adjacent within epsilon, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IntersectsOrAdjacent(
            const OpenClosedInterval<OtherType> other, const T epsilon) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());
            return end_inclusive_ >= (other_start - epsilon) && other_end >= (start_exclusive_ - epsilon);
        }

        /// @brief Calculate the distance to another interval.
        ///
        /// The distance is defined as the minimum distance between the two intervals.
        /// If the intervals intersect or touch, the distance is 0.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to calculate the distance to.
        ///
        /// @return The distance value.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T DistanceTo(
            const OpenClosedInterval<OtherType> other) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());
            if (IntersectsOrAdjacent(other))
            {
                return T{0};
            }
            if (end_inclusive_ < other_start)
            {
                return other_start - end_inclusive_;
            }
            return start_exclusive_ - other_end;
        }

        /// @brief Merges this interval with another interval.
        ///
        /// Creates a new interval spanning the minimum start and maximum end of both intervals,
        /// but only if they intersect or are adjacent.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnValueType The value type of the resulting interval.
        /// @param other The other interval to merge with.
        ///
        /// @return An optional containing the merged interval, or std::nullopt if they are disjoint.
        template <typename OtherType, typename ReturnValueType = T>
            requires std::is_arithmetic_v<T> && std::is_arithmetic_v<OtherType> && std::is_arithmetic_v<ReturnValueType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<OpenClosedInterval<ReturnValueType>> Merge(
            const OpenClosedInterval<OtherType> other) const noexcept
        {
            if (!IntersectsOrAdjacent(other))
            {
                return std::nullopt;
            }
            const ReturnValueType start = std::min(static_cast<ReturnValueType>(start_exclusive_),
                                                   static_cast<ReturnValueType>(other.GetStart()));
            const ReturnValueType end = std::max(static_cast<ReturnValueType>(end_inclusive_),
                                                 static_cast<ReturnValueType>(other.GetEnd()));
            return OpenClosedInterval<ReturnValueType>(start, end);
        }

        /// @brief Merges this interval with another interval using tolerance to bridge gaps.
        ///
        /// Creates a new interval spanning the minimum start and maximum end of both intervals,
        /// provided they are connected within the given epsilon.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnValueType The value type of the resulting interval.
        /// @param other The other interval.
        /// @param epsilon The tolerance value used to determine connectivity.
        ///
        /// @return An optional containing the merged interval, or std::nullopt if they are disjoint beyond epsilon.
        template <typename OtherType, typename ReturnValueType = T>
            requires std::is_arithmetic_v<T> && std::is_arithmetic_v<OtherType> && std::is_arithmetic_v<ReturnValueType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<OpenClosedInterval<ReturnValueType>> Merge(
            const OpenClosedInterval<OtherType> other, const T epsilon) const noexcept
        {
            if (!IntersectsOrAdjacent(other, epsilon))
            {
                return std::nullopt;
            }
            const ReturnValueType start = std::min(static_cast<ReturnValueType>(start_exclusive_),
                                                   static_cast<ReturnValueType>(other.GetStart()));
            const ReturnValueType end = std::max(static_cast<ReturnValueType>(end_inclusive_),
                                                 static_cast<ReturnValueType>(other.GetEnd()));
            return OpenClosedInterval<ReturnValueType>(start, end);
        }

        /// @brief Combines this interval with another interval.
        ///
        /// Returns a new interval that encompasses both intervals (the bounding box),
        /// regardless of whether they intersect or not.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnValueType The value type of the resulting interval.
        /// @param other The other interval to combine with.
        ///
        /// @return A new interval representing the bounding range of both inputs.
        template <typename OtherType, typename ReturnValueType = T>
            requires std::is_arithmetic_v<T> && std::is_arithmetic_v<OtherType> && std::is_arithmetic_v<ReturnValueType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE OpenClosedInterval<ReturnValueType> Combine(
            const OpenClosedInterval<OtherType> other) const noexcept
        {
            const ReturnValueType start = std::min(static_cast<ReturnValueType>(start_exclusive_),
                                                   static_cast<ReturnValueType>(other.GetStart()));
            const ReturnValueType end = std::max(static_cast<ReturnValueType>(end_inclusive_),
                                                 static_cast<ReturnValueType>(other.GetEnd()));
            return OpenClosedInterval<ReturnValueType>(start, end);
        }

        /// @brief Computes the intersection of the current interval with another interval.
        ///
        /// This method returns the interval representing the overlapping region.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnValueType The value type of the resulting interval.
        /// @param other The other interval to compute the intersection with.
        ///
        /// @return An optional containing the intersection interval, or std::nullopt if they do not intersect.
        template <typename OtherType, typename ReturnValueType = T>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<OpenClosedInterval<ReturnValueType>> Intersection(
            const OpenClosedInterval<OtherType> other) const noexcept
        {
            const ReturnValueType start = std::max(static_cast<ReturnValueType>(start_exclusive_),
                                                   static_cast<ReturnValueType>(other.GetStart()));
            const ReturnValueType end = std::min(static_cast<ReturnValueType>(end_inclusive_),
                                                 static_cast<ReturnValueType>(other.GetEnd()));
            if (start >= end)
            {
                return std::nullopt;
            }
            return OpenClosedInterval<ReturnValueType>(start, end);
        }

        /// @brief Clamps the current interval to fit within another interval.
        ///
        /// This restricts the start and end of the current interval to not exceed the boundaries of the provided interval.
        ///
        /// @tparam OtherType The type of the boundary interval's endpoints.
        /// @param boundary The interval defining the valid range.
        ///
        /// @return An optional containing the clamped interval, or std::nullopt if the result would be empty.
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
        ///
        /// Checks if the start and end points are exactly equal.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param lhs The left-hand side interval.
        /// @param rhs The right-hand side interval.
        ///
        /// @return \c true if intervals are equal, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<OtherType>
        friend constexpr BASALT_FORCE_INLINE bool operator==(const OpenClosedInterval lhs,
                                                             const OpenClosedInterval<OtherType> rhs) noexcept
        {
            return lhs.GetStart() == rhs.GetStart() && lhs.GetEnd() == rhs.GetEnd();
        }

        /// @brief Compares two intervals for inequality.
        ///
        /// Checks if the start or end points differ.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param lhs The left-hand side interval.
        /// @param rhs The right-hand side interval.
        ///
        /// @return \c true if intervals are not equal, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<OtherType>
        friend constexpr BASALT_FORCE_INLINE bool operator!=(const OpenClosedInterval lhs,
                                                             const OpenClosedInterval<OtherType> rhs) noexcept
        {
            return !(lhs == rhs);
        }

        friend std::ostream& operator<<(std::ostream& os, const OpenClosedInterval interval)
        {
            return os << "(" << interval.GetStart() << ", " << interval.GetEnd() << "]";
        }

        /// @brief Computes the hash value for the interval.
        ///
        /// This function is compatible with the Abseil hashing framework.
        ///
        /// @tparam H The hasher type.
        /// @param h The hasher state.
        /// @param interval The interval to hash.
        ///
        /// @return The updated hasher state.
        template <typename H>
            requires AbslHasher<H>
        friend constexpr BASALT_FORCE_INLINE H AbslHashValue(H h, const OpenClosedInterval interval) noexcept
        {
            return H::combine(std::move(h), interval.GetStart(), interval.GetEnd());
        }

        /// @brief formats the interval for logging or debugging.
        ///
        /// Formats the interval as "(start, end]".
        ///
        /// @tparam Sink The sink type.
        /// @param sink The sink to write to.
        /// @param interval The interval to stringify.
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
    /// This class represents an interval where both the start and end values are inclusive.
    ///
    /// @tparam T The arithmetic type of the interval's endpoints.
    template <typename T>
        requires std::is_arithmetic_v<T>
    class ClosedInterval
    {
    public:
        using ValueType = T;

        /// @brief Default constructor creating an interval [0, 0].
        constexpr BASALT_FORCE_INLINE ClosedInterval() noexcept
            : start_inclusive_(T{0}),
              end_inclusive_(T{0})
        {
        }

        /// @brief Constructs an interval with the given start and end values.
        ///
        /// This constructor initializes the interval with the specified start and end values.
        ///
        /// @param start_inclusive The inclusive start of the interval.
        /// @param end_inclusive The inclusive end of the interval.
        constexpr BASALT_FORCE_INLINE ClosedInterval(const T start_inclusive, const T end_inclusive) noexcept
            : start_inclusive_(start_inclusive),
              end_inclusive_(end_inclusive)
        {
            DCHECK_LE(start_inclusive, end_inclusive);
        }

        /// @brief Gets the inclusive start of the interval.
        ///
        /// This method returns the lower bound of the interval, which is included in the set.
        ///
        /// @return The inclusive start value.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T GetStart() const noexcept
        {
            return start_inclusive_;
        }

        /// @brief Gets the inclusive end of the interval.
        ///
        /// This method returns the upper bound of the interval, which is included in the set.
        ///
        /// @return The inclusive end value.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T GetEnd() const noexcept
        {
            return end_inclusive_;
        }

        /// @brief Computes the midpoint of the interval.
        ///
        /// This method calculates the arithmetic mean of the start and end values.
        /// For floating point types, it returns the exact center. For integer types, it truncates towards zero.
        ///
        /// @tparam ReturnValueType The type of the returned midpoint. Defaults to T.
        ///
        /// @return The midpoint of the interval.
        template <typename ReturnValueType = T>
            requires std::is_arithmetic_v<ReturnValueType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE ReturnValueType Midpoint() const noexcept
        {
            if constexpr (std::is_floating_point_v<ReturnValueType>)
            {
                return (static_cast<ReturnValueType>(start_inclusive_)
                    + static_cast<ReturnValueType>(end_inclusive_)) / ReturnValueType{2};
            }
            return static_cast<ReturnValueType>(start_inclusive_) +
                static_cast<ReturnValueType>(end_inclusive_ - start_inclusive_) / ReturnValueType{2};
        }

        /// @brief Checks if the interval is empty.
        ///
        /// This method checks if the interval contains no points.
        /// For a closed interval [a, b], it is empty if a > b.
        /// Note: Since the constructor sorts inputs, this should normally be false unless manually constructed invalidly.
        ///
        /// @return \c true if the interval is empty, \c false otherwise.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IsEmpty() const noexcept
        {
            return start_inclusive_ > end_inclusive_;
        }

        /// @brief Checks if the interval is empty using a tolerance epsilon.
        ///
        /// This method checks if the interval is logically inverted beyond the epsilon tolerance.
        ///
        /// @param epsilon The tolerance value for checking emptiness.
        ///
        /// @return \c true if the interval is effectively empty considering tolerance, \c false otherwise.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IsEmpty(const T epsilon) const noexcept
        {
            return (end_inclusive_ - start_inclusive_) < -epsilon;
        }

        /// @brief Gets the length of the interval.
        ///
        /// This method calculates the difference between the end and start values.
        ///
        /// @return The length of the interval.
        [[nodiscard]] constexpr T Length() const noexcept
        {
            return end_inclusive_ - start_inclusive_;
        }

        /// @brief Checks if the interval contains the given value.
        ///
        /// This method checks if the value is within [start, end].
        ///
        /// @tparam OtherType The type of the value to check.
        /// @param value The value to check.
        ///
        /// @return \c true if the value is contained, \c false otherwise.
        template <typename OtherType>
            requires std::convertible_to<OtherType, T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Contains(OtherType value) const noexcept
        {
            return value >= start_inclusive_ && value <= end_inclusive_;
        }

        /// @brief Checks if the interval contains the given value with tolerance.
        ///
        /// This method checks if the value is within [start - epsilon, end + epsilon].
        ///
        /// @tparam OtherType The type of the value to check.
        /// @param value The value to check.
        /// @param epsilon The tolerance buffer to expand the interval boundaries.
        ///
        /// @return \c true if the value is contained within the expanded interval, \c false otherwise.
        template <typename OtherType>
            requires std::convertible_to<OtherType, T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Contains(OtherType value, const T epsilon) const noexcept
        {
            return value >= (start_inclusive_ - epsilon) && value <= (end_inclusive_ + epsilon);
        }

        /// @brief Checks if the interval fully contains another interval.
        ///
        /// This method checks if the other interval is a subset of this interval.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        ///
        /// @return \c true if this interval contains the other interval, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool ContainsInterval(
            const ClosedInterval<OtherType> other) const noexcept
        {
            return other.GetStart() >= start_inclusive_ && other.GetEnd() <= end_inclusive_;
        }

        /// @brief Checks if the interval fully contains another interval with tolerance.
        ///
        /// This method checks if the other interval is a subset of this interval, expanded by epsilon.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @param epsilon The tolerance value used to expand this interval's boundaries.
        ///
        /// @return \c true if this interval (expanded) contains the other interval, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool ContainsInterval(const ClosedInterval<OtherType> other,
                                                                          const T epsilon) const noexcept
        {
            return other.GetStart() >= (start_inclusive_ - epsilon) &&
                other.GetEnd() <= (end_inclusive_ + epsilon);
        }

        /// @brief Checks if the current interval intersects with another interval.
        ///
        /// This method determines if there is any overlap between the two intervals.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        ///
        /// @return \c true if the intervals intersect, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Intersects(
            const ClosedInterval<OtherType> other) const noexcept
        {
            return std::max(start_inclusive_, static_cast<T>(other.GetStart())) <=
                std::min(end_inclusive_, static_cast<T>(other.GetEnd()));
        }

        /// @brief Checks if the current interval intersects with another interval with tolerance.
        ///
        /// This method determines if there is any overlap between the two intervals, allowing for a fuzzy intersection.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @param epsilon The tolerance value.
        ///
        /// @return \c true if the intervals intersect within epsilon, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Intersects(const ClosedInterval<OtherType> other,
                                                                    const T epsilon) const noexcept
        {
            return std::max(start_inclusive_, static_cast<T>(other.GetStart())) <=
                std::min(end_inclusive_, static_cast<T>(other.GetEnd())) + epsilon;
        }

        /// @brief Checks if this interval is adjacent to another interval.
        ///
        /// Adjacency implies the intervals touch exactly at an endpoint. For closed intervals, this also implies intersection at that point.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        ///
        /// @return \c true if the intervals are adjacent, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Adjacent(const ClosedInterval<OtherType> other) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());
            return end_inclusive_ == other_start || start_inclusive_ == other_end;
        }

        /// @brief Checks if this interval is adjacent to another interval with tolerance.
        ///
        /// This checks if the endpoints of the two intervals are within epsilon of each other.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @param epsilon The tolerance value.
        ///
        /// @return \c true if the intervals are adjacent within epsilon, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Adjacent(const ClosedInterval<OtherType> other,
                                                                  const T epsilon) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());

            auto diff1 = (end_inclusive_ > other_start)
                             ? (end_inclusive_ - other_start)
                             : (other_start - end_inclusive_);
            auto diff2 = (start_inclusive_ > other_end)
                             ? (start_inclusive_ - other_end)
                             : (other_end - start_inclusive_);

            return diff1 <= epsilon || diff2 <= epsilon;
        }

        /// @brief Checks if this interval intersects or touches another interval.
        ///
        /// This checks if the two intervals are connected (non-disjoint).
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        ///
        /// @return \c true if the intervals intersect or touch, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IntersectsOrAdjacent(
            const ClosedInterval<OtherType> other) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());
            return end_inclusive_ >= other_start && other_end >= start_inclusive_;
        }

        /// @brief Checks if this interval intersects or touches another interval with tolerance.
        ///
        /// This checks if the two intervals are connected within an epsilon tolerance.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @param epsilon The tolerance value.
        ///
        /// @return \c true if the intervals intersect or are adjacent within epsilon, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IntersectsOrAdjacent(
            const ClosedInterval<OtherType> other, const T epsilon) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());
            return end_inclusive_ >= (other_start - epsilon) && other_end >= (start_inclusive_ - epsilon);
        }

        /// @brief Calculate the distance to another interval.
        ///
        /// The distance is defined as the minimum distance between the two intervals.
        /// If the intervals intersect, the distance is 0.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to calculate the distance to.
        ///
        /// @return The distance value.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T DistanceTo(const ClosedInterval<OtherType> other) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());
            if (IntersectsOrAdjacent(other))
            {
                return T{0};
            }
            if (end_inclusive_ < other_start)
            {
                return other_start - end_inclusive_;
            }
            return start_inclusive_ - other_end;
        }

        /// @brief Merges this interval with another interval.
        ///
        /// Creates a new interval spanning the minimum start and maximum end of both intervals,
        /// but only if they intersect or are adjacent.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnValueType The value type of the resulting interval.
        /// @param other The other interval to merge with.
        ///
        /// @return An optional containing the merged interval, or std::nullopt if they are disjoint.
        template <typename OtherType, typename ReturnValueType = T>
            requires std::is_arithmetic_v<T> && std::is_arithmetic_v<OtherType> && std::is_arithmetic_v<ReturnValueType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<ClosedInterval<ReturnValueType>> Merge(
            const ClosedInterval<OtherType> other) const noexcept
        {
            if (!IntersectsOrAdjacent(other))
            {
                return std::nullopt;
            }
            const ReturnValueType start = std::min(static_cast<ReturnValueType>(start_inclusive_),
                                                   static_cast<ReturnValueType>(other.GetStart()));
            const ReturnValueType end = std::max(static_cast<ReturnValueType>(end_inclusive_),
                                                 static_cast<ReturnValueType>(other.GetEnd()));
            return ClosedInterval<ReturnValueType>(start, end);
        }

        /// @brief Merges this interval with another interval using tolerance to bridge gaps.
        ///
        /// Creates a new interval spanning the minimum start and maximum end of both intervals,
        /// provided they are connected within the given epsilon.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnValueType The value type of the resulting interval.
        /// @param other The other interval.
        /// @param epsilon The tolerance value used to determine connectivity.
        ///
        /// @return An optional containing the merged interval, or std::nullopt if they are disjoint beyond epsilon.
        template <typename OtherType, typename ReturnValueType = T>
            requires std::is_arithmetic_v<T> && std::is_arithmetic_v<OtherType> && std::is_arithmetic_v<ReturnValueType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<ClosedInterval<ReturnValueType>> Merge(
            const ClosedInterval<OtherType> other, const T epsilon) const noexcept
        {
            if (!IntersectsOrAdjacent(other, epsilon))
            {
                return std::nullopt;
            }
            const ReturnValueType start = std::min(static_cast<ReturnValueType>(start_inclusive_),
                                                   static_cast<ReturnValueType>(other.GetStart()));
            const ReturnValueType end = std::max(static_cast<ReturnValueType>(end_inclusive_),
                                                 static_cast<ReturnValueType>(other.GetEnd()));
            return ClosedInterval<ReturnValueType>(start, end);
        }

        /// @brief Combines this interval with another interval.
        ///
        /// Returns a new interval that encompasses both intervals (the bounding box),
        /// regardless of whether they intersect or not.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnValueType The value type of the resulting interval.
        /// @param other The other interval to combine with.
        ///
        /// @return A new interval representing the bounding range of both inputs.
        template <typename OtherType, typename ReturnValueType = T>
            requires std::is_arithmetic_v<T> && std::is_arithmetic_v<OtherType> && std::is_arithmetic_v<ReturnValueType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE ClosedInterval<ReturnValueType> Combine(
            const ClosedInterval<OtherType> other) const noexcept
        {
            const ReturnValueType start = std::min(static_cast<ReturnValueType>(start_inclusive_),
                                                   static_cast<ReturnValueType>(other.GetStart()));
            const ReturnValueType end = std::max(static_cast<ReturnValueType>(end_inclusive_),
                                                 static_cast<ReturnValueType>(other.GetEnd()));
            return ClosedInterval<ReturnValueType>(start, end);
        }

        /// @brief Computes the intersection of the current interval with another interval.
        ///
        /// This method returns the interval representing the overlapping region.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnValueType The value type of the resulting interval.
        /// @param other The other interval to compute the intersection with.
        ///
        /// @return An optional containing the intersection interval, or std::nullopt if they do not intersect.
        template <typename OtherType, typename ReturnValueType = T>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<ClosedInterval<ReturnValueType>> Intersection(
            const ClosedInterval<OtherType> other) const noexcept
        {
            const ReturnValueType start = std::max(static_cast<ReturnValueType>(start_inclusive_),
                                                   static_cast<ReturnValueType>(other.GetStart()));
            const ReturnValueType end = std::min(static_cast<ReturnValueType>(end_inclusive_),
                                                 static_cast<ReturnValueType>(other.GetEnd()));
            if (start > end)
            {
                return std::nullopt;
            }
            return ClosedInterval<ReturnValueType>(start, end);
        }

        /// @brief Clamps the current interval to fit within another interval.
        ///
        /// This restricts the start and end of the current interval to not exceed the boundaries of the provided interval.
        ///
        /// @tparam OtherType The type of the boundary interval's endpoints.
        /// @param boundary The interval defining the valid range.
        ///
        /// @return An optional containing the clamped interval, or std::nullopt if the result would be empty.
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
        ///
        /// Checks if the start and end points are exactly equal.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param lhs The left-hand side interval.
        /// @param rhs The right-hand side interval.
        ///
        /// @return \c true if intervals are equal, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<OtherType>
        friend constexpr BASALT_FORCE_INLINE bool operator==(const ClosedInterval lhs,
                                                             const ClosedInterval<OtherType> rhs) noexcept
        {
            return lhs.GetStart() == rhs.GetStart() && lhs.GetEnd() == rhs.GetEnd();
        }

        /// @brief Compares two intervals for inequality.
        ///
        /// Checks if the start or end points differ.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param lhs The left-hand side interval.
        /// @param rhs The right-hand side interval.
        ///
        /// @return \c true if intervals are not equal, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<OtherType>
        friend constexpr BASALT_FORCE_INLINE bool operator!=(const ClosedInterval lhs,
                                                             const ClosedInterval<OtherType> rhs) noexcept
        {
            return !(lhs == rhs);
        }

        friend std::ostream& operator<<(std::ostream& os, const ClosedInterval interval)
        {
            return os << "[" << interval.GetStart() << ", " << interval.GetEnd() << "]";
        }

        /// @brief Computes the hash value for the interval.
        ///
        /// This function is compatible with the Abseil hashing framework.
        ///
        /// @tparam H The hasher type.
        /// @param h The hasher state.
        /// @param interval The interval to hash.
        ///
        /// @return The updated hasher state.
        template <typename H>
            requires AbslHasher<H>
        friend constexpr BASALT_FORCE_INLINE H AbslHashValue(H h, const ClosedInterval interval) noexcept
        {
            return H::combine(std::move(h), interval.GetStart(), interval.GetEnd());
        }

        /// @brief formats the interval for logging or debugging.
        ///
        /// Formats the interval as "[start, end]".
        ///
        /// @tparam Sink The sink type.
        /// @param sink The sink to write to.
        /// @param interval The interval to stringify.
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
    /// This class represents an interval where both the start and end values are exclusive.
    ///
    /// @tparam T The arithmetic type of the interval's endpoints.
    template <typename T>
        requires std::is_arithmetic_v<T>
    class OpenInterval
    {
    public:
        using ValueType = T;

        /// @brief Default constructor creating an interval (0, 0).
        constexpr BASALT_FORCE_INLINE OpenInterval() noexcept
            : start_exclusive_(T{0}),
              end_exclusive_(T{0})
        {
        }

        /// @brief Constructs an interval with the given start and end values.
        ///
        /// This constructor initializes the interval with the specified start and end values.
        ///
        /// @param start_exclusive The exclusive start of the interval.
        /// @param end_exclusive The exclusive end of the interval.
        constexpr BASALT_FORCE_INLINE OpenInterval(const T start_exclusive, const T end_exclusive) noexcept
            : start_exclusive_(start_exclusive),
              end_exclusive_(end_exclusive)
        {
            DCHECK_LE(start_exclusive, end_exclusive);
        }

        /// @brief Gets the exclusive start of the interval.
        ///
        /// This method returns the lower bound of the interval, which is excluded from the set.
        ///
        /// @return The exclusive start value.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T GetStart() const noexcept
        {
            return start_exclusive_;
        }

        /// @brief Gets the exclusive end of the interval.
        ///
        /// This method returns the upper bound of the interval, which is excluded from the set.
        ///
        /// @return The exclusive end value.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T GetEnd() const noexcept
        {
            return end_exclusive_;
        }

        /// @brief Computes the midpoint of the interval.
        ///
        /// This method calculates the arithmetic mean of the start and end values.
        /// For floating point types, it returns the exact center. For integer types, it truncates towards zero.
        ///
        /// @tparam ReturnValueType The type of the returned midpoint. Defaults to T.
        ///
        /// @return The midpoint of the interval.
        template <typename ReturnValueType = T>
            requires std::is_arithmetic_v<ReturnValueType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE ReturnValueType Midpoint() const noexcept
        {
            if constexpr (std::is_floating_point_v<ReturnValueType>)
            {
                return (static_cast<ReturnValueType>(start_exclusive_)
                    + static_cast<ReturnValueType>(end_exclusive_)) / ReturnValueType{2};
            }
            return static_cast<ReturnValueType>(start_exclusive_) +
                static_cast<ReturnValueType>(end_exclusive_ - start_exclusive_) / ReturnValueType{2};
        }

        /// @brief Checks if the interval is empty.
        ///
        /// This method checks if the interval contains no points.
        /// For an open interval (a, b), it is empty if a >= b.
        ///
        /// @return \c true if the interval is empty, \c false otherwise.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IsEmpty() const noexcept
        {
            return start_exclusive_ >= end_exclusive_;
        }

        /// @brief Checks if the interval is empty using a tolerance epsilon.
        ///
        /// This method checks if the length of the interval is less than or equal to the given epsilon.
        ///
        /// @param epsilon The tolerance value for checking emptiness.
        ///
        /// @return \c true if the interval length is <= epsilon, \c false otherwise.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IsEmpty(const T epsilon) const noexcept
        {
            return (end_exclusive_ - start_exclusive_) <= epsilon;
        }

        /// @brief Gets the length of the interval.
        ///
        /// This method calculates the difference between the end and start values.
        ///
        /// @return The length of the interval.
        [[nodiscard]] constexpr T Length() const noexcept
        {
            return end_exclusive_ - start_exclusive_;
        }

        /// @brief Checks if the interval contains the given value.
        ///
        /// This method checks if the value is within (start, end).
        ///
        /// @tparam OtherType The type of the value to check.
        /// @param value The value to check.
        ///
        /// @return \c true if the value is contained, \c false otherwise.
        template <typename OtherType>
            requires std::convertible_to<OtherType, T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Contains(OtherType value) const noexcept
        {
            return value > start_exclusive_ && value < end_exclusive_;
        }

        /// @brief Checks if the interval contains the given value with tolerance.
        ///
        /// This method checks if the value is within (start - epsilon, end + epsilon).
        ///
        /// @tparam OtherType The type of the value to check.
        /// @param value The value to check.
        /// @param epsilon The tolerance buffer to expand the interval boundaries.
        ///
        /// @return \c true if the value is contained within the expanded interval, \c false otherwise.
        template <typename OtherType>
            requires std::convertible_to<OtherType, T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Contains(OtherType value, const T epsilon) const noexcept
        {
            return value > (start_exclusive_ - epsilon) && value < (end_exclusive_ + epsilon);
        }

        /// @brief Checks if the interval fully contains another interval.
        ///
        /// This method checks if the other interval is a subset of this interval.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        ///
        /// @return \c true if this interval contains the other interval, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool ContainsInterval(
            const OpenInterval<OtherType> other) const noexcept
        {
            return other.GetStart() >= start_exclusive_ && other.GetEnd() <= end_exclusive_;
        }

        /// @brief Checks if the interval fully contains another interval with tolerance.
        ///
        /// This method checks if the other interval is a subset of this interval, expanded by epsilon.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @param epsilon The tolerance value used to expand this interval's boundaries.
        ///
        /// @return \c true if this interval (expanded) contains the other interval, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool ContainsInterval(const OpenInterval<OtherType> other,
                                                                          const T epsilon) const noexcept
        {
            return other.GetStart() >= (start_exclusive_ - epsilon) &&
                other.GetEnd() <= (end_exclusive_ + epsilon);
        }

        /// @brief Checks if the current interval intersects with another interval.
        ///
        /// This method determines if there is any overlap between the two intervals.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        ///
        /// @return \c true if the intervals intersect, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Intersects(const OpenInterval<OtherType> other) const noexcept
        {
            return std::max(start_exclusive_, static_cast<T>(other.GetStart())) <
                std::min(end_exclusive_, static_cast<T>(other.GetEnd()));
        }

        /// @brief Checks if the current interval intersects with another interval with tolerance.
        ///
        /// This method determines if there is any overlap between the two intervals, allowing for a fuzzy intersection.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @param epsilon The tolerance value.
        ///
        /// @return \c true if the intervals intersect within epsilon, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Intersects(const OpenInterval<OtherType> other,
                                                                    const T epsilon) const noexcept
        {
            return std::max(start_exclusive_, static_cast<T>(other.GetStart())) <
                std::min(end_exclusive_, static_cast<T>(other.GetEnd())) + epsilon;
        }

        /// @brief Checks if this interval is adjacent to another interval.
        ///
        /// Adjacency implies the intervals touch exactly at an endpoint but do not strictly overlap.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        ///
        /// @return \c true if the intervals are adjacent, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Adjacent(const OpenInterval<OtherType> other) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());
            return end_exclusive_ == other_start || start_exclusive_ == other_end;
        }

        /// @brief Checks if this interval is adjacent to another interval with tolerance.
        ///
        /// This checks if the endpoints of the two intervals are within epsilon of each other.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @param epsilon The tolerance value.
        ///
        /// @return \c true if the intervals are adjacent within epsilon, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Adjacent(const OpenInterval<OtherType> other,
                                                                  const T epsilon) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());

            auto diff1 = (end_exclusive_ > other_start)
                             ? (end_exclusive_ - other_start)
                             : (other_start - end_exclusive_);
            auto diff2 = (start_exclusive_ > other_end)
                             ? (start_exclusive_ - other_end)
                             : (other_end - start_exclusive_);

            return diff1 <= epsilon || diff2 <= epsilon;
        }

        /// @brief Checks if this interval intersects or touches another interval.
        ///
        /// This checks if the two intervals are connected (non-disjoint).
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        ///
        /// @return \c true if the intervals intersect or touch, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IntersectsOrAdjacent(
            const OpenInterval<OtherType> other) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());
            return end_exclusive_ >= other_start && other_end >= start_exclusive_;
        }

        /// @brief Checks if this interval intersects or touches another interval with tolerance.
        ///
        /// This checks if the two intervals are connected within an epsilon tolerance.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        /// @param epsilon The tolerance value.
        ///
        /// @return \c true if the intervals intersect or are adjacent within epsilon, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IntersectsOrAdjacent(
            const OpenInterval<OtherType> other, const T epsilon) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());
            return end_exclusive_ >= (other_start - epsilon) && other_end >= (start_exclusive_ - epsilon);
        }

        /// @brief Calculate the distance to another interval.
        ///
        /// The distance is defined as the minimum distance between the two intervals.
        /// If the intervals intersect or touch, the distance is 0.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to calculate the distance to.
        ///
        /// @return The distance value.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T DistanceTo(const OpenInterval<OtherType> other) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());
            if (IntersectsOrAdjacent(other))
            {
                return T{0};
            }
            if (end_exclusive_ <= other_start)
            {
                return other_start - end_exclusive_;
            }
            return start_exclusive_ - other_end;
        }

        /// @brief Merges this interval with another interval.
        ///
        /// Creates a new interval spanning the minimum start and maximum end of both intervals,
        /// but only if they intersect or are adjacent.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnValueType The value type of the resulting interval.
        /// @param other The other interval to merge with.
        ///
        /// @return An optional containing the merged interval, or std::nullopt if they are disjoint.
        template <typename OtherType, typename ReturnValueType = T>
            requires std::is_arithmetic_v<T> && std::is_arithmetic_v<OtherType> && std::is_arithmetic_v<ReturnValueType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<OpenInterval<ReturnValueType>> Merge(
            const OpenInterval<OtherType> other) const noexcept
        {
            if (!IntersectsOrAdjacent(other))
            {
                return std::nullopt;
            }
            const ReturnValueType start = std::min(static_cast<ReturnValueType>(start_exclusive_),
                                                   static_cast<ReturnValueType>(other.GetStart()));
            const ReturnValueType end = std::max(static_cast<ReturnValueType>(end_exclusive_),
                                                 static_cast<ReturnValueType>(other.GetEnd()));
            return OpenInterval<ReturnValueType>(start, end);
        }

        /// @brief Merges this interval with another interval using tolerance to bridge gaps.
        ///
        /// Creates a new interval spanning the minimum start and maximum end of both intervals,
        /// provided they are connected within the given epsilon.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnValueType The value type of the resulting interval.
        /// @param other The other interval.
        /// @param epsilon The tolerance value used to determine connectivity.
        ///
        /// @return An optional containing the merged interval, or std::nullopt if they are disjoint beyond epsilon.
        template <typename OtherType, typename ReturnValueType = T>
            requires std::is_arithmetic_v<T> && std::is_arithmetic_v<OtherType> && std::is_arithmetic_v<ReturnValueType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<OpenInterval<ReturnValueType>> Merge(
            const OpenInterval<OtherType> other, const T epsilon) const noexcept
        {
            if (!IntersectsOrAdjacent(other, epsilon))
            {
                return std::nullopt;
            }
            const ReturnValueType start = std::min(static_cast<ReturnValueType>(start_exclusive_),
                                                   static_cast<ReturnValueType>(other.GetStart()));
            const ReturnValueType end = std::max(static_cast<ReturnValueType>(end_exclusive_),
                                                 static_cast<ReturnValueType>(other.GetEnd()));
            return OpenInterval<ReturnValueType>(start, end);
        }

        /// @brief Combines this interval with another interval.
        ///
        /// Returns a new interval that encompasses both intervals (the bounding box),
        /// regardless of whether they intersect or not.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnValueType The value type of the resulting interval.
        /// @param other The other interval to combine with.
        ///
        /// @return A new interval representing the bounding range of both inputs.
        template <typename OtherType, typename ReturnValueType = T>
            requires std::is_arithmetic_v<T> && std::is_arithmetic_v<OtherType> && std::is_arithmetic_v<ReturnValueType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE OpenInterval<ReturnValueType> Combine(
            const OpenInterval<OtherType> other) const noexcept
        {
            const ReturnValueType start = std::min(static_cast<ReturnValueType>(start_exclusive_),
                                                   static_cast<ReturnValueType>(other.GetStart()));
            const ReturnValueType end = std::max(static_cast<ReturnValueType>(end_exclusive_),
                                                 static_cast<ReturnValueType>(other.GetEnd()));
            return OpenInterval<ReturnValueType>(start, end);
        }

        /// @brief Computes the intersection of the current interval with another interval.
        ///
        /// This method returns the interval representing the overlapping region.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnValueType The value type of the resulting interval.
        /// @param other The other interval to compute the intersection with.
        ///
        /// @return An optional containing the intersection interval, or std::nullopt if they do not intersect.
        template <typename OtherType, typename ReturnValueType = T>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<OpenInterval<ReturnValueType>> Intersection(
            const OpenInterval<OtherType> other) const noexcept
        {
            const ReturnValueType start = std::max(static_cast<ReturnValueType>(start_exclusive_),
                                                   static_cast<ReturnValueType>(other.GetStart()));
            const ReturnValueType end = std::min(static_cast<ReturnValueType>(end_exclusive_),
                                                 static_cast<ReturnValueType>(other.GetEnd()));
            if (start >= end)
            {
                return std::nullopt;
            }
            return OpenInterval<ReturnValueType>(start, end);
        }

        /// @brief Clamps the current interval to fit within another interval.
        ///
        /// This restricts the start and end of the current interval to not exceed the boundaries of the provided interval.
        ///
        /// @tparam OtherType The type of the boundary interval's endpoints.
        /// @param boundary The interval defining the valid range.
        ///
        /// @return An optional containing the clamped interval, or std::nullopt if the result would be empty.
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
        ///
        /// Checks if the start and end points are exactly equal.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param lhs The left-hand side interval.
        /// @param rhs The right-hand side interval.
        ///
        /// @return \c true if intervals are equal, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<OtherType>
        friend constexpr BASALT_FORCE_INLINE bool operator==(const OpenInterval lhs,
                                                             const OpenInterval<OtherType> rhs) noexcept
        {
            return lhs.GetStart() == rhs.GetStart() && lhs.GetEnd() == rhs.GetEnd();
        }

        /// @brief Compares two intervals for inequality.
        ///
        /// Checks if the start or end points differ.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param lhs The left-hand side interval.
        /// @param rhs The right-hand side interval.
        ///
        /// @return \c true if intervals are not equal, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<OtherType>
        friend constexpr BASALT_FORCE_INLINE bool operator!=(const OpenInterval lhs,
                                                             const OpenInterval<OtherType> rhs) noexcept
        {
            return !(lhs == rhs);
        }

        friend std::ostream& operator<<(std::ostream& os, const OpenInterval interval)
        {
            return os << "(" << interval.GetStart() << ", " << interval.GetEnd() << ")";
        }

        /// @brief Computes the hash value for the interval.
        ///
        /// This function is compatible with the Abseil hashing framework.
        ///
        /// @tparam H The hasher type.
        /// @param h The hasher state.
        /// @param interval The interval to hash.
        ///
        /// @return The updated hasher state.
        template <typename H>
            requires AbslHasher<H>
        friend constexpr BASALT_FORCE_INLINE H AbslHashValue(H h, const OpenInterval interval) noexcept
        {
            return H::combine(std::move(h), interval.GetStart(), interval.GetEnd());
        }

        /// @brief formats the interval for logging or debugging.
        ///
        /// Formats the interval as "(start, end)".
        ///
        /// @tparam Sink The sink type.
        /// @param sink The sink to write to.
        /// @param interval The interval to stringify.
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
} // namespace bslt

namespace std
{
    // hash specializations for bslt interval types
    // for use in std containers like std::unordered_map/set

    template <typename T>
    struct hash<bslt::ClosedOpenInterval<T>>
    {
        BASALT_FORCE_INLINE size_t operator()(const bslt::ClosedOpenInterval<T> interval) const noexcept
        {
            return absl::Hash<bslt::ClosedOpenInterval<T>>{}(interval);
        }
    };

    template <typename T>
    struct hash<bslt::OpenClosedInterval<T>>
    {
        BASALT_FORCE_INLINE size_t operator()(const bslt::OpenClosedInterval<T> interval) const noexcept
        {
            return absl::Hash<bslt::OpenClosedInterval<T>>{}(interval);
        }
    };

    template <typename T>
    struct hash<bslt::ClosedInterval<T>>
    {
        BASALT_FORCE_INLINE size_t operator()(const bslt::ClosedInterval<T> interval) const noexcept
        {
            return absl::Hash<bslt::ClosedInterval<T>>{}(interval);
        }
    };

    template <typename T>
    struct hash<bslt::OpenInterval<T>>
    {
        BASALT_FORCE_INLINE size_t operator()(const bslt::OpenInterval<T> interval) const noexcept
        {
            return absl::Hash<bslt::OpenInterval<T>>{}(interval);
        }
    };
} // namespace std

#endif // BASALT_MATH_INTERVAL_H_
