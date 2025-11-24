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

#ifndef BASALT_CONTAINER_INTERVAL_SET_H_
#define BASALT_CONTAINER_INTERVAL_SET_H_

#include <vector>
#include <iterator>
#include <algorithm>
#include <utility>
#include <optional>
#include <ranges>
#include <concepts>
#include <type_traits>
#include "absl/container/btree_set.h"
#include "basalt/math/interval.h"

namespace bslt
{
    /// @brief Defines the underlying storage type for an IntervalSet.
    enum class IntervalSetStorage
    {
        /// @brief Use a std::vector as the underlying storage.
        kVector,

        /// @brief Use an absl::btree_set as the underlying storage.
        kBTree,
    };

    /// @brief A set of disjoint, sorted intervals [start, end).
    ///
    /// This set is templated on the underlying value type (e.g., int, float, double).
    /// It strictly manages canonical `ClosedOpenInterval<T>` types.
    ///
    /// All operations maintain the invariant that intervals are sorted and disjoint.
    ///
    /// Adjacent intervals (e.g., [0, 1) and [1, 2)) are automatically merged.
    ///
    /// @tparam T The type of the interval's endpoints.
    /// @tparam Storage The storage backend.
    template <typename T, IntervalSetStorage Storage = IntervalSetStorage::kVector>
        requires std::is_arithmetic_v<T>
    class IntervalSet;

    /// @brief An implementation of IntervalSet using a `std::vector`.
    ///
    /// This implementation maintains a `std::vector` of `ClosedOpenInterval<T>`
    /// intervals that is always kept sorted and disjoint. Best for sets that are
    /// iterated frequently but modified less often.
    ///
    /// @tparam T The type of the interval's endpoints.
    template <typename T>
        requires std::is_arithmetic_v<T>
    class IntervalSet<T, IntervalSetStorage::kVector>
    {
    public:
        /// @brief The underlying type of the interval endpoints.
        using ValueType = T;
        /// @brief The canonical interval type used for internal storage, `[start, end)`.
        using IntervalType = ClosedOpenInterval<ValueType>;
        /// @brief The underlying container type.
        using container_type = std::vector<IntervalType>;
        /// @brief A non-const iterator to the underlying container.
        using iterator = container_type::iterator;
        /// @brief A const iterator to the underlying container.
        using const_iterator = container_type::const_iterator;
        /// @brief The size type.
        using size_type = container_type::size_type;
        /// @brief The standard value type definition.
        using value_type = IntervalType;
        /// @brief The standard difference type definition.
        using difference_type = container_type::difference_type;
        /// @brief The standard pointer type definition.
        using pointer = container_type::const_pointer;
        /// @brief The standard const pointer type definition.
        using const_pointer = container_type::const_pointer;
        /// @brief The standard reference type definition.
        using reference = container_type::const_reference;
        /// @brief The standard const reference type definition.
        using const_reference = container_type::const_reference;
        /// @brief A reverse iterator to the underlying container.
        using reverse_iterator = container_type::const_reverse_iterator;
        /// @brief A const reverse iterator to the underlying container.
        using const_reverse_iterator = container_type::const_reverse_iterator;

        /// @brief Constructs an empty IntervalSet.
        constexpr BASALT_FORCE_INLINE IntervalSet() noexcept = default;

        /// @brief Constructs an empty IntervalSet with reserved capacity.
        ///
        /// @param capacity The number of intervals to reserve space for.
        explicit BASALT_FORCE_INLINE IntervalSet(const size_type capacity)
        {
            intervals_.reserve(capacity);
        }

        /// @brief Constructs an IntervalSet from a range of intervals.
        ///
        /// Performs a bulk load optimization: collects all intervals, sorts them,
        /// and performs a single-pass merge. This is O(N log N) compared to O(N^2)
        /// for repeated insertion into a vector.
        ///
        /// @param r A range.
        template <std::ranges::input_range R>
            requires std::convertible_to<std::ranges::range_value_t<R>, IntervalType>
        explicit IntervalSet(R&& r)
        {
            if constexpr (std::ranges::sized_range<R>)
            {
                intervals_.reserve(std::ranges::size(r));
            }

            for (auto&& interval : r)
            {
                if (!interval.IsEmpty())
                {
                    intervals_.push_back(interval);
                }
            }

            if (intervals_.empty())
            {
                return;
            }

            std::ranges::sort(intervals_, [](const IntervalType& a, const IntervalType& b)
            {
                return a.GetStart() < b.GetStart();
            });

            size_type write_idx = 0;
            for (size_type read_idx = 1; read_idx < intervals_.size(); ++read_idx)
            {
                if (intervals_[write_idx].IntersectsOrAdjacent(intervals_[read_idx]))
                {
                    intervals_[write_idx] = intervals_[write_idx].Combine(intervals_[read_idx]);
                }
                else
                {
                    ++write_idx;
                    if (write_idx != read_idx)
                    {
                        intervals_[write_idx] = intervals_[read_idx];
                    }
                }
            }
            intervals_.erase(intervals_.begin() + write_idx + 1, intervals_.end());
        }

        /// @brief Inserts a range of intervals into the set.
        ///
        /// @tparam R A range satisfying std::ranges::input_range.
        template <std::ranges::input_range R>
            requires std::convertible_to<std::ranges::range_value_t<R>, IntervalType>
        void InsertRange(R&& r)
        {
            for (auto&& interval : r)
            {
                Insert(interval);
            }
        }

        /// @brief Returns a constant iterator to the beginning of the set.
        ///
        /// @return A const_iterator pointing to the first interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator begin() const noexcept
        {
            return intervals_.begin();
        }

        /// @brief Returns a constant iterator to the end of the set.
        ///
        /// @return A const_iterator pointing one past the last interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator end() const noexcept
        {
            return intervals_.end();
        }

        /// @brief Returns a constant iterator to the beginning of the set.
        ///
        /// @return A const_iterator pointing to the first interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator cbegin() const noexcept
        {
            return intervals_.cbegin();
        }

        /// @brief Returns a constant iterator to the end of the set.
        ///
        /// @return A const_iterator pointing one past the last interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator cend() const noexcept
        {
            return intervals_.cend();
        }

        /// @brief Returns a reverse iterator to the beginning of the reversed set.
        [[nodiscard]] BASALT_FORCE_INLINE const_reverse_iterator rbegin() const noexcept
        {
            return intervals_.rbegin();
        }

        /// @brief Returns a reverse iterator to the end of the reversed set.
        [[nodiscard]] BASALT_FORCE_INLINE const_reverse_iterator rend() const noexcept
        {
            return intervals_.rend();
        }

        /// @brief Returns a constant reverse iterator to the beginning of the reversed set.
        [[nodiscard]] BASALT_FORCE_INLINE const_reverse_iterator crbegin() const noexcept
        {
            return intervals_.crbegin();
        }

        /// @brief Returns a constant reverse iterator to the end of the reversed set.
        [[nodiscard]] BASALT_FORCE_INLINE const_reverse_iterator crend() const noexcept
        {
            return intervals_.crend();
        }

        [[nodiscard]] BASALT_FORCE_INLINE const_iterator begin() noexcept
        {
            return intervals_.begin();
        }

        [[nodiscard]] BASALT_FORCE_INLINE const_iterator end() noexcept
        {
            return intervals_.end();
        }

        [[nodiscard]] BASALT_FORCE_INLINE const_reverse_iterator rbegin() noexcept
        {
            return intervals_.rbegin();
        }

        [[nodiscard]] BASALT_FORCE_INLINE const_reverse_iterator rend() noexcept
        {
            return intervals_.rend();
        }

        /// @brief Checks if the set is empty.
        ///
        /// @return \c true if the set contains no intervals, \c false otherwise.
        [[nodiscard]] BASALT_FORCE_INLINE bool IsEmpty() const noexcept
        {
            return intervals_.empty();
        }

        /// @brief Returns the number of disjoint intervals in the set.
        ///
        /// @return The number of intervals.
        [[nodiscard]] BASALT_FORCE_INLINE size_type size() const noexcept
        {
            return intervals_.size();
        }

        /// @brief Clears all intervals from the set.
        BASALT_FORCE_INLINE void clear() noexcept
        {
            intervals_.clear();
        }

        /// @brief Inserts a new interval into the set.
        ///
        /// The new interval will be merged with any existing intervals it overlaps or is adjacent to.
        ///
        /// @param new_interval The interval `[start, end)` to insert.
        void Insert(IntervalType new_interval)
        {
            if (new_interval.IsEmpty())
            {
                return;
            }

            auto it = lower_bound(new_interval.GetStart());
            IntervalType merged_interval = new_interval;
            auto erase_start = it;

            while (it != intervals_.end() && merged_interval.IntersectsOrAdjacent(*it))
            {
                merged_interval = merged_interval.Combine(*it);
                ++it;
            }

            if (erase_start != it)
            {
                *erase_start = merged_interval;
                auto erase_tail_begin = std::next(erase_start);
                if (erase_tail_begin != it)
                {
                    intervals_.erase(erase_tail_begin, it);
                }
            }
            else
            {
                intervals_.insert(it, merged_interval);
            }
        }

        /// @brief Inserts a new interval into the set using epsilon tolerance.
        ///
        /// This allows intervals that are separated by a gap smaller than or equal to
        /// `epsilon` to be merged into a single interval.
        ///
        /// @param new_interval The interval to insert.
        /// @param epsilon The tolerance for adjacency and intersection.
        void Insert(IntervalType new_interval, const ValueType epsilon)
        {
            if (new_interval.IsEmpty(epsilon))
            {
                return;
            }

            auto it = lower_bound(sub_with_floor(new_interval.GetStart(), epsilon));
            IntervalType merged_interval = new_interval;
            auto erase_start = it;

            while (it != intervals_.end() && merged_interval.IntersectsOrAdjacent(*it, epsilon))
            {
                DCHECK(merged_interval.IntersectsOrAdjacent(*it, epsilon)) << "Attempting to merge disjoint intervals";

                merged_interval = merged_interval.Combine(*it);
                ++it;
            }

            DCHECK_LE(merged_interval.GetStart(), new_interval.GetStart());
            DCHECK_GE(merged_interval.GetEnd(), new_interval.GetEnd());

            if (erase_start != it)
            {
                *erase_start = merged_interval;
                auto erase_tail_begin = std::next(erase_start);
                if (erase_tail_begin != it)
                {
                    intervals_.erase(erase_tail_begin, it);
                }
            }
            else
            {
                intervals_.insert(it, merged_interval);
            }
        }

        /// @brief Erases an interval from the set.
        ///
        /// This operation will "subtract" the `interval_to_remove` from all
        /// intervals in the set, handling shrinking and splitting in-place.
        ///
        /// @param interval_to_remove The interval `[start, end)` to subtract.
        void Erase(IntervalType interval_to_remove)
        {
            if (interval_to_remove.IsEmpty())
            {
                return;
            }

            auto it = lower_bound(interval_to_remove.GetStart());
            if (it == intervals_.end() || !interval_to_remove.Intersects(*it))
            {
                return;
            }

            auto erase_start = it;

            std::optional<IntervalType> first_frag;
            if (it->GetStart() < interval_to_remove.GetStart())
            {
                first_frag = IntervalType(it->GetStart(), interval_to_remove.GetStart());
            }

            auto erase_end = it;
            while (erase_end != intervals_.end() && interval_to_remove.Intersects(*erase_end))
            {
                ++erase_end;
            }
            auto last_intersected_it = std::prev(erase_end);

            std::optional<IntervalType> last_frag;
            if (interval_to_remove.GetEnd() < last_intersected_it->GetEnd())
            {
                last_frag = IntervalType(interval_to_remove.GetEnd(), last_intersected_it->GetEnd());
            }

            if (std::distance(erase_start, erase_end) == 1)
            {
                if (first_frag.has_value() && last_frag.has_value())
                {
                    *erase_start = *first_frag;
                    intervals_.insert(std::next(erase_start), *last_frag);
                }
                else if (first_frag.has_value())
                {
                    *erase_start = *first_frag;
                }
                else if (last_frag.has_value())
                {
                    *erase_start = *last_frag;
                }
                else
                {
                    intervals_.erase(erase_start);
                }
                return;
            }

            if (first_frag.has_value() || last_frag.has_value())
            {
                const IntervalType replacement =
                    first_frag.has_value() ? *first_frag : *last_frag;

                *erase_start = replacement;

                auto erase_tail_begin = std::next(erase_start);
                if (erase_tail_begin != erase_end)
                {
                    intervals_.erase(erase_tail_begin, erase_end);
                }

                if (first_frag.has_value() && last_frag.has_value())
                {
                    intervals_.insert(std::next(erase_start), *last_frag);
                }
            }
            else
            {
                intervals_.erase(erase_start, erase_end);
            }
        }

        /// @brief Erases an interval using epsilon tolerance for intersection checks.
        ///
        /// Note: The strict bounds of `interval_to_remove` are still used for the cut,
        /// but `epsilon` determines *which* intervals are considered touched/intersected.
        ///
        /// @param interval_to_remove The interval to subtract.
        /// @param epsilon The tolerance for checking intersections.
        void Erase(IntervalType interval_to_remove, const ValueType epsilon)
        {
            if (interval_to_remove.IsEmpty(epsilon))
            {
                return;
            }

            auto it = lower_bound(sub_with_floor(interval_to_remove.GetStart(), epsilon));
            if (it == intervals_.end() || !interval_to_remove.Intersects(*it, epsilon))
            {
                return;
            }

            auto erase_start = it;

            std::optional<IntervalType> first_frag;
            if (it->GetStart() < interval_to_remove.GetStart())
            {
                first_frag = IntervalType(it->GetStart(), interval_to_remove.GetStart());
            }

            auto erase_end = it;
            while (erase_end != intervals_.end() && interval_to_remove.Intersects(*erase_end, epsilon))
            {
                ++erase_end;
            }
            auto last_intersected_it = std::prev(erase_end);

            std::optional<IntervalType> last_frag;
            if (interval_to_remove.GetEnd() < last_intersected_it->GetEnd())
            {
                last_frag = IntervalType(interval_to_remove.GetEnd(), last_intersected_it->GetEnd());
            }

            if (std::distance(erase_start, erase_end) == 1)
            {
                if (first_frag.has_value() && last_frag.has_value())
                {
                    *erase_start = *first_frag;
                    intervals_.insert(std::next(erase_start), *last_frag);
                }
                else if (first_frag.has_value())
                {
                    *erase_start = *first_frag;
                }
                else if (last_frag.has_value())
                {
                    *erase_start = *last_frag;
                }
                else
                {
                    intervals_.erase(erase_start);
                }
                return;
            }

            if (first_frag.has_value() || last_frag.has_value())
            {
                const IntervalType replacement =
                    first_frag.has_value() ? *first_frag : *last_frag;

                *erase_start = replacement;

                auto erase_tail_begin = std::next(erase_start);
                if (erase_tail_begin != erase_end)
                {
                    intervals_.erase(erase_tail_begin, erase_end);
                }

                if (first_frag.has_value() && last_frag.has_value())
                {
                    intervals_.insert(std::next(erase_start), *last_frag);
                }
            }
            else
            {
                intervals_.erase(erase_start, erase_end);
            }
        }

        /// @brief Adds all intervals from another set to this set (set union).
        ///
        /// @param other The other IntervalSet to insert.
        void Insert(const IntervalSet& other)
        {
            if (other.IsEmpty())
            {
                return;
            }
            if (this->IsEmpty())
            {
                intervals_ = other.intervals_;
                return;
            }

            container_type new_intervals;
            new_intervals.reserve(intervals_.size() + other.intervals_.size());

            auto it_this = intervals_.begin();
            auto it_other = other.intervals_.begin();

            std::optional<IntervalType> current_merged;
            if (it_this->GetStart() <= it_other->GetStart())
            {
                current_merged = *it_this;
                ++it_this;
            }
            else
            {
                current_merged = *it_other;
                ++it_other;
            }

            while (it_this != intervals_.end() || it_other != other.intervals_.end())
            {
                const IntervalType* next_interval = nullptr;
                if (it_this == intervals_.end() ||
                    (it_other != other.intervals_.end() && it_other->GetStart() < it_this->GetStart()))
                {
                    next_interval = &(*it_other);
                    ++it_other;
                }
                else
                {
                    next_interval = &(*it_this);
                    ++it_this;
                }

                if (current_merged->IntersectsOrAdjacent(*next_interval))
                {
                    *current_merged = current_merged->Combine(*next_interval);
                }
                else
                {
                    new_intervals.push_back(*current_merged);
                    current_merged = *next_interval;
                }
            }

            new_intervals.push_back(*current_merged);
            intervals_ = std::move(new_intervals);
        }

        /// @brief Subtracts all intervals from another set from this set (set difference).
        ///
        /// @param other The other IntervalSet to subtract.
        void Erase(const IntervalSet& other)
        {
            if (other.IsEmpty() || this->IsEmpty())
            {
                return;
            }

            container_type new_intervals;
            new_intervals.reserve(intervals_.size());

            auto it_this = intervals_.begin();
            auto it_other = other.intervals_.begin();

            std::optional<IntervalType> fragment;
            if (it_this != intervals_.end())
            {
                fragment = *it_this;
                ++it_this;
            }

            while (fragment.has_value())
            {
                while (it_other != other.intervals_.end() && it_other->GetEnd() <= fragment->GetStart())
                {
                    ++it_other;
                }

                if (it_other == other.intervals_.end())
                {
                    new_intervals.push_back(*fragment);
                    new_intervals.insert(new_intervals.end(), it_this, intervals_.end());
                    break;
                }

                if (it_other->GetStart() >= fragment->GetEnd())
                {
                    new_intervals.push_back(*fragment);
                    if (it_this != intervals_.end())
                    {
                        fragment = *it_this;
                        ++it_this;
                    }
                    else
                    {
                        fragment.reset();
                    }
                    continue;
                }

                if (fragment->GetStart() < it_other->GetStart())
                {
                    new_intervals.push_back(IntervalType(fragment->GetStart(), it_other->GetStart()));
                }

                if (fragment->GetEnd() > it_other->GetEnd())
                {
                    fragment = IntervalType(it_other->GetEnd(), fragment->GetEnd());
                    ++it_other;
                }
                else
                {
                    if (it_this != intervals_.end())
                    {
                        fragment = *it_this;
                        ++it_this;
                    }
                    else
                    {
                        fragment.reset();
                    }
                }
            }

            intervals_ = std::move(new_intervals);
        }

        /// @brief Intersects this set with another set.
        ///
        /// Modifies this set to contain only the intervals that are present in both sets.
        ///
        /// @param other The other IntervalSet to intersect with.
        void Intersection(const IntervalSet& other)
        {
            container_type new_intervals;
            new_intervals.reserve(std::min(intervals_.size(), other.intervals_.size()));

            auto it1 = intervals_.begin();
            auto it2 = other.intervals_.begin();

            while (it1 != intervals_.end() && it2 != other.intervals_.end())
            {
                if (auto opt_i = it1->Intersection(*it2); opt_i.has_value())
                {
                    if (!new_intervals.empty() && new_intervals.back().IntersectsOrAdjacent(opt_i.value()))
                    {
                        new_intervals.back() = new_intervals.back().Combine(opt_i.value());
                    }
                    else
                    {
                        new_intervals.push_back(opt_i.value());
                    }
                }

                if (it1->GetEnd() < it2->GetEnd())
                {
                    ++it1;
                }
                else
                {
                    ++it2;
                }
            }
            intervals_ = std::move(new_intervals);
        }

        /// @brief Clips the set to the given boundary `[start, end)`.
        ///
        /// Removes all parts of the set that lie outside the boundary interval.
        ///
        /// @param boundary The interval defining the clipping region.
        void Clip(IntervalType boundary)
        {
            if (boundary.IsEmpty())
            {
                clear();
                DCHECK(IsEmpty());
                return;
            }

            auto it_first = lower_bound(boundary.GetStart());

            constexpr auto comp_start = [](const IntervalType& i, ValueType v)
            {
                return i.GetStart() < v;
            };
            auto it_last = std::lower_bound(intervals_.begin(), intervals_.end(), boundary.GetEnd(), comp_start);

            if (it_first == intervals_.end() || it_first >= it_last)
            {
                clear();
                return;
            }

            if (it_first != intervals_.begin())
            {
                auto new_end = std::move(it_first, it_last, intervals_.begin());
                intervals_.erase(new_end, intervals_.end());
            }
            else
            {
                intervals_.erase(it_last, intervals_.end());
            }

            if (!intervals_.empty())
            {
                auto opt_first = intervals_.front().Intersection(boundary);
                if (opt_first.has_value())
                {
                    intervals_.front() = *opt_first;
                }
                else
                {
                    intervals_.erase(intervals_.begin());
                }
            }

            if (!intervals_.empty())
            {
                auto opt_last = intervals_.back().Intersection(boundary);
                if (opt_last.has_value())
                {
                    intervals_.back() = *opt_last;
                }
                else
                {
                    intervals_.pop_back();
                }
            }
        }

        /// @brief Adds all intervals from another set to this set (union).
        ///
        /// @param other The other IntervalSet.
        /// @return A reference to this IntervalSet.
        BASALT_FORCE_INLINE IntervalSet& operator+=(const IntervalSet& other)
        {
            Insert(other);
            return *this;
        }

        /// @brief Subtracts all intervals from another set from this set (difference).
        ///
        /// @param other The other IntervalSet.
        /// @return A reference to this IntervalSet.
        BASALT_FORCE_INLINE IntervalSet& operator-=(const IntervalSet& other)
        {
            Erase(other);
            return *this;
        }

        /// @brief Intersects this set with another set.
        ///
        /// @param other The other IntervalSet.
        /// @return A reference to this IntervalSet.
        BASALT_FORCE_INLINE IntervalSet& operator&=(const IntervalSet& other)
        {
            Intersection(other);
            return *this;
        }

        /// @brief Computes the union of two IntervalSets.
        ///
        /// @param lhs The left-hand side IntervalSet.
        /// @param rhs The right-hand side IntervalSet.
        /// @return A new IntervalSet containing the union.
        friend BASALT_FORCE_INLINE IntervalSet operator+(IntervalSet lhs, const IntervalSet& rhs)
        {
            lhs += rhs;
            return lhs;
        }

        /// @brief Computes the difference of two IntervalSets.
        ///
        /// @param lhs The left-hand side IntervalSet.
        /// @param rhs The right-hand side IntervalSet.
        /// @return A new IntervalSet containing the difference.
        friend BASALT_FORCE_INLINE IntervalSet operator-(IntervalSet lhs, const IntervalSet& rhs)
        {
            lhs -= rhs;
            return lhs;
        }

        /// @brief Computes the intersection of two IntervalSets.
        ///
        /// @param lhs The left-hand side IntervalSet.
        /// @param rhs The right-hand side IntervalSet.
        /// @return A new IntervalSet containing the intersection.
        friend BASALT_FORCE_INLINE IntervalSet operator&(IntervalSet lhs, const IntervalSet& rhs)
        {
            lhs &= rhs;
            return lhs;
        }

        /// @brief Checks if a specific value is contained in the set.
        ///
        /// @param v The value to check.
        /// @return \c true if the value is contained, \c false otherwise.
        [[nodiscard]] BASALT_FORCE_INLINE bool Contains(ValueType v) const noexcept
        {
            auto candidate = find_candidate_for_value(v);
            if (candidate == intervals_.end())
            {
                return false;
            }
            return v < candidate->GetEnd();
        }

        /// @brief Checks if a specific value is contained in the set with epsilon tolerance.
        ///
        /// @param v The value to check.
        /// @param epsilon The tolerance value.
        /// @return \c true if the value is contained within epsilon, \c false otherwise.
        [[nodiscard]] bool Contains(ValueType v, const ValueType epsilon) const noexcept
        {
            auto candidate = find_candidate_for_value(v);
            if (candidate != intervals_.end())
            {
                if (candidate->Contains(v, epsilon))
                {
                    return true;
                }
            }
            auto it_check = lower_bound(sub_with_floor(v, epsilon));
            if (it_check != intervals_.end() && it_check->Contains(v, epsilon))
            {
                return true;
            }

            return false;
        }

        /// @brief Checks if an interval is fully contained in the set.
        ///
        /// @param other The interval to check.
        /// @return \c true if the interval is a subset of the set, \c false otherwise.
        [[nodiscard]] bool ContainsInterval(const IntervalType& other) const noexcept
        {
            if (other.IsEmpty())
            {
                return true;
            }
            auto candidate = find_candidate_for_value(other.GetStart());
            if (candidate == intervals_.end())
            {
                return false;
            }
            return other.GetEnd() <= candidate->GetEnd();
        }

        /// @brief Checks if an interval is fully contained using epsilon tolerance.
        ///
        /// @param other The interval to check.
        /// @param epsilon The tolerance value.
        /// @return \c true if the interval is a subset within epsilon, \c false otherwise.
        [[nodiscard]] bool ContainsInterval(const IntervalType& other,
                                            const ValueType epsilon) const noexcept
        {
            if (other.IsEmpty(epsilon))
            {
                return true;
            }
            auto it = lower_bound(sub_with_floor(other.GetStart(), epsilon));
            if (it == intervals_.end())
            {
                return false;
            }
            return it->ContainsInterval(other, epsilon);
        }

        /// @brief Checks if the set has any overlap with a given interval.
        ///
        /// @param other The interval to check.
        /// @return \c true if there is an overlap, \c false otherwise.
        [[nodiscard]] BASALT_FORCE_INLINE bool Intersects(const IntervalType& other) const noexcept
        {
            if (other.IsEmpty())
            {
                return false;
            }

            auto it = lower_bound(other.GetStart());
            if (it == intervals_.end())
            {
                return false;
            }

            return it->Intersects(other);
        }

        /// @brief Checks if the set has any overlap with a given interval using epsilon tolerance.
        ///
        /// @param other The interval to check.
        /// @param epsilon The tolerance value.
        /// @return \c true if there is an overlap within epsilon, \c false otherwise.
        [[nodiscard]] BASALT_FORCE_INLINE bool Intersects(const IntervalType& other,
                                                          const ValueType epsilon) const noexcept
        {
            if (other.IsEmpty(epsilon))
            {
                return false;
            }

            auto it = lower_bound(sub_with_floor(other.GetStart(), epsilon));
            if (it == intervals_.end())
            {
                return false;
            }

            return it->Intersects(other, epsilon);
        }

        friend BASALT_FORCE_INLINE bool operator==(const IntervalSet& lhs, const IntervalSet& rhs)
        {
            return lhs.intervals_ == rhs.intervals_;
        }

        friend BASALT_FORCE_INLINE bool operator!=(const IntervalSet& lhs, const IntervalSet& rhs)
        {
            return !(lhs == rhs);
        }

        /// @brief Finds the first interval `it` such that `it.GetEnd() >= v`. (const)
        ///
        /// @param v The value to search for.
        /// @return A const_iterator to the first candidate, or `end()`.
        BASALT_FORCE_INLINE const_iterator lower_bound(ValueType v) const noexcept
        {
            constexpr auto comp = [](const IntervalType& i, ValueType value)
            {
                return i.GetEnd() < value;
            };
            return std::lower_bound(intervals_.cbegin(), intervals_.cend(), v, comp);
        }

        BASALT_FORCE_INLINE iterator lower_bound(ValueType v) noexcept
        {
            constexpr auto comp = [](const IntervalType& i, ValueType value)
            {
                return i.GetEnd() < value;
            };
            return std::lower_bound(intervals_.begin(), intervals_.end(), v, comp);
        }

    private:
        /// @brief Subtracts rhs from lhs with floor at zero for unsigned types.
        ///
        /// @param lhs The left-hand side value.
        /// @param rhs The right-hand side value.
        ///
        /// @return The result of lhs - rhs, floored at zero for unsigned types.
        static constexpr BASALT_FORCE_INLINE ValueType sub_with_floor(const ValueType lhs, const ValueType rhs) noexcept
        {
            if constexpr (std::is_unsigned_v<ValueType>)
            {
                return (rhs > lhs) ? ValueType(0) : ValueType(lhs - rhs);
            }
            else
            {
                // Fast path for signed types.
                return lhs - rhs;
            }
        }

        /// @brief Finds the interval that *could* contain `v`.
        ///
        /// Returns the interval strictly containing `v` or `end()` if none.
        ///
        /// @param v The value to search for.
        /// @return A const_iterator to the interval containing `v`.
        BASALT_FORCE_INLINE const_iterator find_candidate_for_value(ValueType v) const noexcept
        {
            constexpr auto comp = [](ValueType v, const IntervalType& i)
            {
                return v < i.GetStart();
            };
            auto it = std::upper_bound(intervals_.begin(), intervals_.end(), v, comp);
            if (it == intervals_.begin())
            {
                return intervals_.end();
            }
            return std::prev(it);
        }

        container_type intervals_;
    };

    /// @brief An implementation of IntervalSet using `absl::btree_set`.
    ///
    /// This storage is optimized for frequent insertions and lookups in sets with a large number of disjoint intervals.
    ///
    /// @tparam T The type of the interval's endpoints.
    template <typename T>
        requires std::is_arithmetic_v<T>
    class IntervalSet<T, IntervalSetStorage::kBTree>
    {
        // Unlike a standard set, this class maintains a strictly disjoint and non-adjacent state.
        // The internal `absl::btree_set` uses a specialized comparator that treats adjacent intervals
        // as equal. Therefore, all mutation methods (`Insert`, `Erase`) strictly enforce that
        // overlapping or adjacent intervals are merged or split before interacting with the
        // underlying BTree structure.
        //
        // Breaking this invariant (e.g., by forcing two adjacent intervals into the underlying container)
        // will corrupt the tree's search capabilities.

    public:
        /// @brief The underlying type of the interval endpoints.
        using ValueType = T;
        /// @brief The canonical interval type used for internal storage, `[start, end)`.
        using IntervalType = ClosedOpenInterval<ValueType>;

    private:
        /// @brief Custom comparer for storing disjoint intervals in the BTree.
        /// Defines ordering `a < b` if `a.End <= b.Start`.
        struct ClosedOpenIntervalComparer
        {
            /// @brief Custom comparer for storing disjoint intervals in the BTree.
            ///
            /// @note **CRITICAL INVARIANT**: This comparator relies on the set strictly containing
            /// disjoint and non-adjacent intervals.
            ///
            /// This defines a strict partial ordering where `a < b` if `a` is strictly to the left of `b`.
            /// - `[0, 1) < [2, 3)` is TRUE.
            /// - `[0, 1) < [1, 2)` is FALSE (Adjacent).
            /// - `[1, 2) < [0, 1)` is FALSE.
            ///
            /// Consequently, the BTree considers adjacent or overlapping intervals to be **EQUAL**.
            /// This is safe ONLY because `IntervalSet` guarantees that adjacent intervals are
            /// merged *before* they are inserted into the tree.
            ///
            /// @param a The left-hand side interval.
            /// @param b The right-hand side interval.
            /// @return \c true if `a` comes strictly before `b`.
            constexpr BASALT_FORCE_INLINE bool operator()(const IntervalType& a,
                                                          const IntervalType& b) const noexcept
            {
                return a.GetEnd() < b.GetStart();
            }

            /// @brief Enables transparent comparison for heterogeneous lookups.
            using is_transparent = void;
        };

    public:
        /// @brief The underlying container type.
        using container_type = absl::btree_set<IntervalType, ClosedOpenIntervalComparer>;
        /// @brief A const iterator to the underlying container.
        using const_iterator = container_type::const_iterator;
        /// @brief The iterator type.
        using iterator = container_type::const_iterator;
        /// @brief The size type.
        using size_type = container_type::size_type;
        /// @brief The standard value type definition for range compatibility.
        using value_type = IntervalType;
        /// @brief The difference type.
        using difference_type = container_type::difference_type;
        /// @brief The pointer type.
        using pointer = container_type::const_pointer;
        /// @brief The const pointer type.
        using const_pointer = container_type::const_pointer;
        /// @brief The reference type.
        using reference = container_type::const_reference;
        /// @brief The const reference type.
        using const_reference = container_type::const_reference;
        /// @brief The reverse iterator type.
        using reverse_iterator = container_type::const_reverse_iterator;
        /// @brief The const reverse iterator type.
        using const_reverse_iterator = container_type::const_reverse_iterator;

        /// @brief Constructs an empty IntervalSet.
        constexpr BASALT_FORCE_INLINE IntervalSet() noexcept = default;

        /// @brief Constructs an empty IntervalSet with a hint for capacity.
        ///
        /// Note that `absl::btree_set` does not support `reserve` in the same way as `std::vector`,
        /// so this constructor is provided for API compatibility.
        ///
        /// @param capacity The expected number of intervals.
        explicit BASALT_FORCE_INLINE IntervalSet([[maybe_unused]] const size_type capacity)
        {
        }

        /// @brief Constructs an IntervalSet from a range of intervals.
        template <std::ranges::input_range R>
            requires std::convertible_to<std::ranges::range_value_t<R>, IntervalType>
        explicit IntervalSet(R&& r)
        {
            for (auto&& interval : r)
            {
                Insert(interval);
            }
        }

        /// @brief Inserts a range of intervals into the set.
        ///
        /// @tparam R A range satisfying std::ranges::input_range.
        template <std::ranges::input_range R>
            requires std::convertible_to<std::ranges::range_value_t<R>, IntervalType>
        void InsertRange(R&& r)
        {
            for (auto&& interval : r)
            {
                Insert(interval);
            }
        }

        /// @brief Returns a constant iterator to the beginning of the set.
        ///
        /// @return A const_iterator pointing to the first interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator begin() const noexcept
        {
            return intervals_.begin();
        }

        /// @brief Returns a constant iterator to the end of the set.
        ///
        /// @return A const_iterator pointing one past the last interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator end() const noexcept
        {
            return intervals_.end();
        }

        /// @brief Returns a constant iterator to the beginning of the set.
        ///
        /// @return A const_iterator pointing to the first interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator cbegin() const noexcept
        {
            return intervals_.cbegin();
        }

        /// @brief Returns a constant iterator to the end of the set.
        ///
        /// @return A const_iterator pointing one past the last interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator cend() const noexcept
        {
            return intervals_.cend();
        }

        /// @brief Returns a reverse iterator to the beginning of the reversed set.
        [[nodiscard]] BASALT_FORCE_INLINE const_reverse_iterator rbegin() const noexcept
        {
            return intervals_.rbegin();
        }

        /// @brief Returns a reverse iterator to the end of the reversed set.
        [[nodiscard]] BASALT_FORCE_INLINE const_reverse_iterator rend() const noexcept
        {
            return intervals_.rend();
        }

        /// @brief Returns a constant reverse iterator to the beginning of the reversed set.
        [[nodiscard]] BASALT_FORCE_INLINE const_reverse_iterator crbegin() const noexcept
        {
            return intervals_.crbegin();
        }

        /// @brief Returns a constant reverse iterator to the end of the reversed set.
        [[nodiscard]] BASALT_FORCE_INLINE const_reverse_iterator crend() const noexcept
        {
            return intervals_.crend();
        }

        /// @brief Returns a non-const iterator to the beginning of the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator begin() noexcept
        {
            return intervals_.begin();
        }

        /// @brief Returns a non-const iterator to the end of the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator end() noexcept
        {
            return intervals_.end();
        }

        /// @brief Returns a reverse iterator to the beginning of the reversed set.
        [[nodiscard]] BASALT_FORCE_INLINE const_reverse_iterator rbegin() noexcept
        {
            return intervals_.rbegin();
        }

        /// @brief Returns a reverse iterator to the end of the reversed set.
        [[nodiscard]] BASALT_FORCE_INLINE const_reverse_iterator rend() noexcept
        {
            return intervals_.rend();
        }

        /// @brief Checks if the set is empty.
        ///
        /// @return \c true if the set contains no intervals, \c false otherwise.
        [[nodiscard]] BASALT_FORCE_INLINE bool IsEmpty() const noexcept
        {
            return intervals_.empty();
        }

        /// @brief Returns the number of disjoint intervals in the set.
        ///
        /// @return The number of intervals.
        [[nodiscard]] BASALT_FORCE_INLINE size_type size() const noexcept
        {
            return intervals_.size();
        }

        /// @brief Clears all intervals from the set.
        BASALT_FORCE_INLINE void clear() noexcept
        {
            intervals_.clear();
        }

        /// @brief Inserts a new interval into the set.
        ///
        /// The new interval will be merged with any existing intervals it overlaps or is adjacent to.
        ///
        /// @param new_interval The interval `[start, end)` to insert.
        void Insert(IntervalType new_interval)
        {
            if (new_interval.IsEmpty())
            {
                return;
            }

            auto it = lower_bound(new_interval.GetStart());
            IntervalType merged_interval = new_interval;
            auto erase_start = it;

            while (it != intervals_.end() && merged_interval.IntersectsOrAdjacent(*it))
            {
                merged_interval = merged_interval.Combine(*it);
                ++it;
            }

            if (erase_start != it)
            {
                it = intervals_.erase(erase_start, it);
            }
            intervals_.insert(it, merged_interval);
        }

        /// @brief Inserts a new interval into the set using epsilon tolerance.
        ///
        /// This allows intervals that are separated by a gap smaller than or equal to
        /// `epsilon` to be merged into a single interval.
        ///
        /// @param new_interval The interval to insert.
        /// @param epsilon The tolerance for adjacency and intersection.
        void Insert(IntervalType new_interval, const ValueType epsilon)
        {
            if (new_interval.IsEmpty(epsilon))
            {
                return;
            }

            // Search for start - epsilon
            auto it = lower_bound(sub_with_floor(new_interval.GetStart(), epsilon));
            IntervalType merged_interval = new_interval;
            auto erase_start = it;

            // Merge based on epsilon connectivity
            while (it != intervals_.end() && merged_interval.IntersectsOrAdjacent(*it, epsilon))
            {
                DCHECK(merged_interval.IntersectsOrAdjacent(*it, epsilon)) << "Attempting to merge disjoint intervals";

                merged_interval = merged_interval.Combine(*it);
                ++it;
            }

            DCHECK_LE(merged_interval.GetStart(), new_interval.GetStart());
            DCHECK_GE(merged_interval.GetEnd(), new_interval.GetEnd());

            if (erase_start != it)
            {
                it = intervals_.erase(erase_start, it);
            }

            intervals_.insert(it, merged_interval);
        }

        /// @brief Adds all intervals from another set to this set (set union).
        ///
        /// @param other The other IntervalSet to insert.
        void Insert(const IntervalSet& other)
        {
            if (other.IsEmpty())
            {
                return;
            }
            if (this->IsEmpty())
            {
                intervals_ = other.intervals_;
                return;
            }

            std::vector<IntervalType> linear_buffer;
            // High upper bound on size. We will most likely not need this much.
            linear_buffer.reserve(intervals_.size() + other.intervals_.size());

            auto it_this = intervals_.begin();
            auto it_other = other.intervals_.begin();

            bool start_with_this = (it_this->GetStart() <= it_other->GetStart());
            IntervalType current_merged = start_with_this ? *it_this : *it_other;

            if (start_with_this)
            {
                ++it_this;
            }
            else
            {
                ++it_other;
            }

            while (it_this != intervals_.end() || it_other != other.intervals_.end())
            {
                const IntervalType* next_interval = nullptr;

                if (it_this == intervals_.end() ||
                    (it_other != other.intervals_.end() && it_other->GetStart() < it_this->GetStart()))
                {
                    next_interval = &(*it_other);
                    ++it_other;
                }
                else
                {
                    next_interval = &(*it_this);
                    ++it_this;
                }

                if (current_merged.IntersectsOrAdjacent(*next_interval))
                {
                    current_merged = current_merged.Combine(*next_interval);
                }
                else
                {
                    linear_buffer.push_back(current_merged);
                    current_merged = *next_interval;
                }
            }
            linear_buffer.push_back(current_merged);
            intervals_ = container_type(linear_buffer.begin(), linear_buffer.end());
        }

        /// @brief Erases an interval from the set.
        ///
        /// This operation will "subtract" the `interval_to_remove` from all
        /// intervals in the set, handling shrinking and splitting in-place.
        ///
        /// @param interval_to_remove The interval `[start, end)` to subtract.
        void Erase(IntervalType interval_to_remove)
        {
            if (interval_to_remove.IsEmpty())
            {
                return;
            }

            auto it = lower_bound(interval_to_remove.GetStart());

            if (it == intervals_.end() || !interval_to_remove.Intersects(*it))
            {
                return;
            }

            auto erase_start = it;

            std::optional<IntervalType> first_frag;
            if (it->GetStart() < interval_to_remove.GetStart())
            {
                first_frag = IntervalType(it->GetStart(), interval_to_remove.GetStart());

                DCHECK_LT(first_frag->GetEnd(), it->GetEnd());
                DCHECK(!first_frag->IsEmpty());
            }

            auto erase_end = it;
            while (erase_end != intervals_.end() && interval_to_remove.Intersects(*erase_end))
            {
                ++erase_end;
            }
            auto last_intersected_it = std::prev(erase_end);

            std::optional<IntervalType> last_frag;
            if (interval_to_remove.GetEnd() < last_intersected_it->GetEnd())
            {
                last_frag = IntervalType(interval_to_remove.GetEnd(), last_intersected_it->GetEnd());
            }

            auto hint = intervals_.erase(erase_start, erase_end);

            if (first_frag.has_value())
            {
                hint = intervals_.insert(hint, *first_frag);
                ++hint;
            }
            if (last_frag.has_value())
            {
                intervals_.insert(hint, *last_frag);
            }
        }

        /// @brief Erases an interval using epsilon tolerance for intersection checks.
        ///
        /// Note: The strict bounds of `interval_to_remove` are still used for the cut,
        /// but `epsilon` determines *which* intervals are considered touched/intersected.
        ///
        /// @param interval_to_remove The interval to subtract.
        /// @param epsilon The tolerance for checking intersections.
        void Erase(IntervalType interval_to_remove, const ValueType epsilon)
        {
            if (interval_to_remove.IsEmpty(epsilon))
            {
                return;
            }

            auto it = lower_bound(sub_with_floor(interval_to_remove.GetStart(), epsilon));

            if (it == intervals_.end() || !interval_to_remove.Intersects(*it, epsilon))
            {
                return;
            }

            auto erase_start = it;

            std::optional<IntervalType> first_frag;
            if (it->GetStart() < interval_to_remove.GetStart())
            {
                first_frag = IntervalType(it->GetStart(), interval_to_remove.GetStart());
            }

            auto erase_end = it;
            while (erase_end != intervals_.end() && interval_to_remove.Intersects(*erase_end, epsilon))
            {
                ++erase_end;
            }
            auto last_intersected_it = std::prev(erase_end);

            std::optional<IntervalType> last_frag;
            if (interval_to_remove.GetEnd() < last_intersected_it->GetEnd())
            {
                last_frag = IntervalType(interval_to_remove.GetEnd(), last_intersected_it->GetEnd());
            }

            auto hint = intervals_.erase(erase_start, erase_end);

            if (first_frag.has_value())
            {
                hint = intervals_.insert(hint, *first_frag);
                ++hint;
            }
            if (last_frag.has_value())
            {
                intervals_.insert(hint, *last_frag);
            }
        }

        /// @brief Subtracts all intervals from another set from this set (set difference).
        ///
        /// @param other The other IntervalSet to subtract.
        void Erase(const IntervalSet& other)
        {
            if (other.IsEmpty() || this->IsEmpty())
            {
                return;
            }

            container_type new_intervals;
            auto it_this = intervals_.begin();
            auto it_other = other.intervals_.begin();

            std::optional<IntervalType> fragment;
            if (it_this != intervals_.end())
            {
                fragment = *it_this;
                ++it_this;
            }

            while (fragment.has_value())
            {
                while (it_other != other.intervals_.end() && it_other->GetEnd() <= fragment->GetStart())
                {
                    ++it_other;
                }

                if (it_other == other.intervals_.end())
                {
                    new_intervals.insert(new_intervals.end(), *fragment);
                    new_intervals.insert(it_this, intervals_.end());
                    break;
                }

                if (it_other->GetStart() >= fragment->GetEnd())
                {
                    new_intervals.insert(new_intervals.end(), *fragment);
                    if (it_this != intervals_.end())
                    {
                        fragment = *it_this;
                        ++it_this;
                    }
                    else
                    {
                        fragment.reset();
                    }
                    continue;
                }

                if (fragment->GetStart() < it_other->GetStart())
                {
                    new_intervals.insert(new_intervals.end(), IntervalType(fragment->GetStart(), it_other->GetStart()));
                }

                if (fragment->GetEnd() > it_other->GetEnd())
                {
                    fragment = IntervalType(it_other->GetEnd(), fragment->GetEnd());
                    ++it_other;
                }
                else
                {
                    if (it_this != intervals_.end())
                    {
                        fragment = *it_this;
                        ++it_this;
                    }
                    else
                    {
                        fragment.reset();
                    }
                }
            }

            intervals_ = std::move(new_intervals);
        }

        /// @brief Intersects this set with another set.
        ///
        /// Modifies this set to contain only the intervals that are present in both sets.
        ///
        /// @param other The other IntervalSet to intersect with.
        void Intersection(const IntervalSet& other)
        {
            container_type new_intervals;

            auto it1 = intervals_.begin();
            auto it2 = other.intervals_.begin();
            std::optional<IntervalType> last_inserted;

            while (it1 != intervals_.end() && it2 != other.intervals_.end())
            {
                if (auto opt_i = it1->Intersection(*it2); opt_i.has_value())
                {
                    if (last_inserted.has_value() && last_inserted->IntersectsOrAdjacent(opt_i.value()))
                    {
                        *last_inserted = last_inserted->Combine(opt_i.value());
                    }
                    else
                    {
                        if (last_inserted.has_value())
                        {
                            new_intervals.insert(new_intervals.end(), *last_inserted);
                        }
                        last_inserted = opt_i.value();
                    }
                }

                if (it1->GetEnd() < it2->GetEnd())
                {
                    ++it1;
                }
                else
                {
                    ++it2;
                }
            }

            if (last_inserted.has_value())
            {
                new_intervals.insert(new_intervals.end(), *last_inserted);
            }

            intervals_ = std::move(new_intervals);
        }

        /// @brief Clips the set to the given boundary `[start, end)`.
        ///
        /// Removes all parts of the set that lie outside the boundary interval.
        ///
        /// @param boundary The interval defining the clipping region.
        void Clip(IntervalType boundary)
        {
            if (boundary.IsEmpty())
            {
                clear();
                DCHECK(IsEmpty());
                return;
            }

            auto it_keep = lower_bound(boundary.GetStart());
            intervals_.erase(intervals_.begin(), it_keep);

            auto it_erase = intervals_.begin();
            while (it_erase != intervals_.end() && it_erase->GetStart() < boundary.GetEnd())
            {
                ++it_erase;
            }
            intervals_.erase(it_erase, intervals_.end());

            if (intervals_.empty())
            {
                return;
            }

            auto first_node = intervals_.extract(intervals_.begin());
            if (auto opt_i = first_node.value().Intersection(boundary); opt_i.has_value())
            {
                first_node.value() = *opt_i;
                intervals_.insert(intervals_.begin(), std::move(first_node));
            }

            if (intervals_.empty())
            {
                return;
            }

            auto last_node = intervals_.extract(std::prev(intervals_.end()));
            if (auto opt_i = last_node.value().Intersection(boundary); opt_i.has_value())
            {
                last_node.value() = *opt_i;
                intervals_.insert(intervals_.end(), std::move(last_node));
            }
        }

        /// @brief Adds all intervals from another set to this set (union).
        ///
        /// @param other The other IntervalSet.
        /// @return A reference to this IntervalSet.
        BASALT_FORCE_INLINE IntervalSet& operator+=(const IntervalSet& other)
        {
            Insert(other);
            return *this;
        }

        /// @brief Subtracts all intervals from another set from this set (difference).
        ///
        /// @param other The other IntervalSet.
        /// @return A reference to this IntervalSet.
        BASALT_FORCE_INLINE IntervalSet& operator-=(const IntervalSet& other)
        {
            Erase(other);
            return *this;
        }

        /// @brief Intersects this set with another set.
        ///
        /// @param other The other IntervalSet.
        /// @return A reference to this IntervalSet.
        BASALT_FORCE_INLINE IntervalSet& operator&=(const IntervalSet& other)
        {
            Intersection(other);
            return *this;
        }

        /// @brief Computes the union of two IntervalSets.
        ///
        /// @param lhs The left-hand side IntervalSet.
        /// @param rhs The right-hand side IntervalSet.
        /// @return A new IntervalSet containing the union.
        friend BASALT_FORCE_INLINE IntervalSet operator+(IntervalSet lhs, const IntervalSet& rhs)
        {
            lhs += rhs;
            return lhs;
        }

        /// @brief Computes the difference of two IntervalSets.
        ///
        /// @param lhs The left-hand side IntervalSet.
        /// @param rhs The right-hand side IntervalSet.
        /// @return A new IntervalSet containing the difference.
        friend BASALT_FORCE_INLINE IntervalSet operator-(IntervalSet lhs, const IntervalSet& rhs)
        {
            lhs -= rhs;
            return lhs;
        }

        /// @brief Computes the intersection of two IntervalSets.
        ///
        /// @param lhs The left-hand side IntervalSet.
        /// @param rhs The right-hand side IntervalSet.
        /// @return A new IntervalSet containing the intersection.
        friend BASALT_FORCE_INLINE IntervalSet operator&(IntervalSet lhs, const IntervalSet& rhs)
        {
            lhs &= rhs;
            return lhs;
        }

        /// @brief Checks if a specific value is contained in the set.
        ///
        /// @param v The value to check.
        /// @return \c true if the value is contained, \c false otherwise.
        [[nodiscard]] BASALT_FORCE_INLINE bool Contains(ValueType v) const noexcept
        {
            // For disjoint intervals stored in a BTree, finding the candidate is tricky with
            // the strict comparator. We use lower_bound to find the first interval
            // that ends *after* v.
            auto it = lower_bound(v);
            if (it == intervals_.end())
            {
                return false;
            }
            // If the found interval starts at or before v, it contains v.
            return it->GetStart() <= v;
        }

        /// @brief Checks if a value is contained using epsilon tolerance.
        ///
        /// @param v The value to check.
        /// @param epsilon The tolerance value.
        /// @return \c true if the value is contained within epsilon, \c false otherwise.
        [[nodiscard]] BASALT_FORCE_INLINE bool Contains(ValueType v, const ValueType epsilon) const noexcept
        {
            auto it = lower_bound(sub_with_floor(v, epsilon));
            if (it == intervals_.end())
            {
                return false;
            }
            return it->Contains(v, epsilon);
        }

        /// @brief Checks if an interval is fully contained in the set.
        ///
        /// @param other The interval to check.
        /// @return \c true if the interval is a subset of the set, \c false otherwise.
        [[nodiscard]] BASALT_FORCE_INLINE bool ContainsInterval(const IntervalType& other) const noexcept
        {
            if (other.IsEmpty())
            {
                return true;
            }
            auto it = lower_bound(other.GetStart());
            if (it == intervals_.end())
            {
                return false;
            }
            return it->GetStart() <= other.GetStart() && it->GetEnd() >= other.GetEnd();
        }

        /// @brief Checks if an interval is fully contained using epsilon tolerance.
        ///
        /// @param other The interval to check.
        /// @param epsilon The tolerance value.
        /// @return \c true if the interval is a subset within epsilon, \c false otherwise.
        [[nodiscard]] BASALT_FORCE_INLINE bool ContainsInterval(const IntervalType& other,
                                                                const ValueType epsilon) const noexcept
        {
            if (other.IsEmpty(epsilon))
            {
                return true;
            }
            auto it = lower_bound(sub_with_floor(other.GetStart(), epsilon));
            if (it == intervals_.end())
            {
                return false;
            }
            return it->ContainsInterval(other, epsilon);
        }

        /// @brief Checks if the set has any overlap with a given interval.
        ///
        /// @param other The interval to check.
        /// @return \c true if there is an overlap, \c false otherwise.
        [[nodiscard]] BASALT_FORCE_INLINE bool Intersects(const IntervalType& other) const noexcept
        {
            if (other.IsEmpty())
            {
                return false;
            }

            auto it = lower_bound(other.GetStart());
            if (it == intervals_.end())
            {
                return false;
            }

            return it->Intersects(other);
        }

        /// @brief Checks if the set has any overlap with a given interval using epsilon tolerance.
        ///
        /// @param other The interval to check.
        /// @param epsilon The tolerance value.
        /// @return \c true if there is an overlap within epsilon, \c false otherwise.
        [[nodiscard]] BASALT_FORCE_INLINE bool Intersects(const IntervalType& other,
                                                          const ValueType epsilon) const noexcept
        {
            if (other.IsEmpty(epsilon))
            {
                return false;
            }

            auto it = lower_bound(sub_with_floor(other.GetStart(), epsilon));
            if (it == intervals_.end())
            {
                return false;
            }

            return it->Intersects(other, epsilon);
        }

        friend BASALT_FORCE_INLINE bool operator==(const IntervalSet& lhs, const IntervalSet& rhs)
        {
            return lhs.intervals_ == rhs.intervals_;
        }

        friend BASALT_FORCE_INLINE bool operator!=(const IntervalSet& lhs, const IntervalSet& rhs)
        {
            return !(lhs == rhs);
        }

        /// @brief Finds the first interval `it` such that `it.GetEnd() > v`. (const)
        ///
        /// @param v The value to search for.
        /// @return A const iterator to the first candidate, or `end()`.
        BASALT_FORCE_INLINE const_iterator lower_bound(const ValueType v) const noexcept
        {
            return intervals_.lower_bound(IntervalType(v, v));
        }

        /// @brief Finds the first interval `it` such that `it.GetEnd() > v`. (const)
        ///
        /// @param v The value to search for.
        /// @return A const iterator to the first candidate, or `end()`.
        BASALT_FORCE_INLINE iterator lower_bound(const ValueType v) noexcept
        {
            return intervals_.lower_bound(IntervalType(v, v));
        }

    private:
        /// @brief Subtracts rhs from lhs with floor at zero for unsigned types.
        ///
        /// @param lhs The left-hand side value.
        /// @param rhs The right-hand side value.
        ///
        /// @return The result of lhs - rhs, floored at zero for unsigned types.
        static constexpr BASALT_FORCE_INLINE ValueType sub_with_floor(const ValueType lhs, const ValueType rhs) noexcept
        {
            if constexpr (std::is_unsigned_v<ValueType>)
            {
                return (rhs > lhs) ? ValueType(0) : ValueType(lhs - rhs);
            }
            else
            {
                // Fast path for signed types.
                return lhs - rhs;
            }
        }

        container_type intervals_;
    };
}

#endif // BASALT_CONTAINER_INTERVAL_SET_H_
