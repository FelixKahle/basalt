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
#include <type_traits>
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
    concept Interval = requires (const T& interval, const T& other, typename T::ValueType v) {
        typename T::ValueType;
        requires std::is_arithmetic_v<typename T::ValueType>;

        // The start value of the interval
        { interval.GetStart() } -> std::convertible_to<typename T::ValueType>;

        // The end value of the interval
        { interval.GetEnd() } -> std::convertible_to<typename T::ValueType>;

        // Check if the interval is empty
        { interval.IsEmpty() } -> std::same_as<bool>;

        // The length of the interval
        { interval.Length() } -> std::convertible_to<typename T::ValueType>;

        // The midpoint of the interval
        { interval.Midpoint() } -> std::convertible_to<typename T::ValueType>;

        // Check if a value is contained within the interval
        { interval.Contains(v) } -> std::same_as<bool>;

        // Check if another interval is contained within this interval
        { interval.ContainsInterval(other) } -> std::same_as<bool>;

        // Check if this interval intersects with another interval.
        // Intersection is defined as having at least one point in common.
        { interval.Intersects(other) } -> std::same_as<bool>;

        // Check if this interval intersects or is adjacent to another interval.
        // This includes overlapping and adjacent cases.
        { interval.IntersectsOrAdjacent(other) } -> std::same_as<bool>;

        // Check if this interval is adjacent to another interval.
        // Adjacency is defined as being next to each other without overlapping.
        { interval.Adjacent(other) } -> std::same_as<bool>;

        // Calculate the distance to another interval.
        // The distance is defined as the minimum distance between the two intervals.
        { interval.DistanceTo(other) } -> std::convertible_to<typename T::ValueType>;

        // Get the intersection of this interval with another interval.
        // Returns an optional interval representing the intersection,
        // or std::nullopt if there is no
        { interval.Intersection(other) } -> std::same_as<std::optional<T>>;

        // Clamp this interval to another interval.
        // Returns an optional interval representing the clamped interval,
        // or std::nullopt if there is no overlap.
        { interval.Clamp(other) } -> std::same_as<std::optional<T>>;

        // Merge this interval with another interval.
        // Returns an optional interval representing the merged interval,
        // or std::nullopt if they cannot be merged because they are not
        // overlapping or touching.
        { interval.Merge(other) } -> std::same_as<std::optional<T>>;

        // Combine this interval with another interval.
        // Returns a new interval that encompasses both intervals,
        // consuming also space that is between them.
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
                return (static_cast<ReturnTimeType>(start_inclusive_)
                    + static_cast<ReturnTimeType>(end_exclusive_)) / ReturnTimeType{2};
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
            return other.start_inclusive_ >= start_inclusive_ && other.end_exclusive_ <= end_exclusive_;
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

        /// @brief Checks if this interval is adjacent to another interval.
        ///
        /// Adjacency is defined as being next to each other without overlapping.
        /// This is synonymous with \c Touches for \c ClosedOpenInterval.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check for adjacency.
        ///
        /// @return \c true if the intervals are adjacent, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool Adjacent(const ClosedOpenInterval<OtherType> other) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());
            return end_exclusive_ == other_start || start_inclusive_ == other_end;
        }

        /// @brief Checks if this interval intersects or touches another interval.
        ///
        /// This checks if the two intervals are not disjoint.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to check.
        ///
        /// @return \c true if the intervals intersect or touch, \c false otherwise.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE bool IntersectsOrAdjacent(const ClosedOpenInterval<OtherType> other) const noexcept
        {
            const T other_start = static_cast<T>(other.GetStart());
            const T other_end = static_cast<T>(other.GetEnd());
            return end_exclusive_ >= other_start && other_end >= start_inclusive_;
        }

        /// @brief Calculate the distance to another interval.
        ///
        /// The distance is defined as the minimum distance between the two
        /// intervals. If the intervals intersect or touch, the distance is 0.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @param other The other interval to calculate the distance to.
        ///
        /// @return The distance to the other interval.
        template <typename OtherType>
            requires std::is_arithmetic_v<T>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE T DistanceTo(const ClosedOpenInterval<OtherType> other) const noexcept
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
        /// Returns an optional interval representing the merged interval,
        /// or std::nullopt if they cannot be merged because they are not
        /// overlapping or touching.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnTimeType The type of the resulting interval's endpoints.
        /// @param other The other interval to merge with.
        /// @return An optional containing the merged interval, or std::nullopt.
        template <typename OtherType, typename ReturnTimeType = T>
            requires std::is_arithmetic_v<T> && std::is_arithmetic_v<OtherType> && std::is_arithmetic_v<ReturnTimeType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE std::optional<ClosedOpenInterval<ReturnTimeType>> Merge(
            const ClosedOpenInterval<OtherType> other) const noexcept
        {
            if (!IntersectsOrAdjacent(other))
            {
                return std::nullopt;
            }

            const ReturnTimeType start = std::min(static_cast<ReturnTimeType>(start_inclusive_),
                                                  static_cast<ReturnTimeType>(other.GetStart()));
            const ReturnTimeType end = std::max(static_cast<ReturnTimeType>(end_exclusive_),
                                                static_cast<ReturnTimeType>(other.GetEnd()));

            return ClosedOpenInterval<ReturnTimeType>(start, end);
        }

        /// @brief Combines this interval with another interval.
        ///
        /// Returns a new interval that encompasses both intervals,
        /// including any space that is between them if they are disjoint.
        ///
        /// @tparam OtherType The type of the other interval's endpoints.
        /// @tparam ReturnTimeType The type of the resulting interval's endpoints.
        /// @param other The other interval to combine with.
        /// @return A new interval that represents the "bounding box" of both.
        template <typename OtherType, typename ReturnTimeType = T>
            requires std::is_arithmetic_v<T> && std::is_arithmetic_v<OtherType> && std::is_arithmetic_v<ReturnTimeType>
        [[nodiscard]] constexpr BASALT_FORCE_INLINE ClosedOpenInterval<ReturnTimeType> Combine(
            const ClosedOpenInterval<OtherType> other) const noexcept
        {
            const ReturnTimeType start = std::min(static_cast<ReturnTimeType>(start_inclusive_),
                                                  static_cast<ReturnTimeType>(other.GetStart()));
            const ReturnTimeType end = std::max(static_cast<ReturnTimeType>(end_exclusive_),
                                                static_cast<ReturnTimeType>(other.GetEnd()));

            return ClosedOpenInterval<ReturnTimeType>(start, end);
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
}

#endif // BASALT_MATH_INTERVAL_H_
