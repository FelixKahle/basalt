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
#include "absl/container/btree_set.h"
#include "basalt/math/interval.h"

namespace bslt
{
    /// @brief Defines the underlying storage type for an IntervalSet.
    enum class IntervalSetStorage
    {
        kVector,
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
    class IntervalSet;

    /// @brief An implementation of IntervalSet using a `std::vector`.
    ///
    /// This implementation maintains a `std::vector` of `ClosedOpenInterval<T>`
    /// intervals that is always kept sorted and disjoint. Best for sets that are
    /// iterated frequently but modified less often.
    ///
    /// @tparam T The type of the interval's endpoints.
    template <typename T>
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

        /// @brief Constructs an empty IntervalSet.
        constexpr BASALT_FORCE_INLINE IntervalSet() noexcept = default;

        /// @brief Constructs an empty IntervalSet with reserved capacity.
        ///
        /// @param capacity The number of intervals to reserve space for.
        explicit BASALT_FORCE_INLINE IntervalSet(const size_type capacity)
        {
            m_intervals.reserve(capacity);
        }

        /// @brief Returns a constant iterator to the beginning of the set.
        ///
        /// @return A const_iterator pointing to the first interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator begin() const noexcept { return m_intervals.begin(); }

        /// @brief Returns a constant iterator to the end of the set.
        ///
        /// @return A const_iterator pointing one past the last interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator end() const noexcept { return m_intervals.end(); }

        /// @brief Returns a constant iterator to the beginning of the set.
        ///
        /// @return A const_iterator pointing to the first interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator cbegin() const noexcept { return m_intervals.cbegin(); }

        /// @brief Returns a constant iterator to the end of the set.
        ///
        /// @return A const_iterator pointing one past the last interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator cend() const noexcept { return m_intervals.cend(); }

        /// @brief Checks if the set is empty.
        ///
        /// @return \c true if the set contains no intervals, \c false otherwise.
        [[nodiscard]] BASALT_FORCE_INLINE bool IsEmpty() const noexcept { return m_intervals.empty(); }

        /// @brief Returns the number of disjoint intervals in the set.
        ///
        /// @return The number of intervals.
        [[nodiscard]] BASALT_FORCE_INLINE size_type size() const noexcept { return m_intervals.size(); }

        /// @brief Clears all intervals from the set.
        BASALT_FORCE_INLINE void clear() noexcept { m_intervals.clear(); }

        /// @brief Inserts a new interval into the set.
        ///
        /// The new interval will be merged with any existing intervals it overlaps or is adjacent to.
        ///
        ///
        /// @param new_interval The interval `[start, end)` to insert.
        BASALT_FORCE_INLINE void Insert(IntervalType new_interval)
        {
            if (new_interval.IsEmpty())
            {
                return;
            }

            auto it = find_first_candidate(new_interval.GetStart());
            IntervalType merged_interval = new_interval;
            auto erase_start = it;

            while (it != m_intervals.end() && merged_interval.IntersectsOrAdjacent(*it))
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
                    m_intervals.erase(erase_tail_begin, it);
                }
            }
            else
            {
                m_intervals.insert(it, merged_interval);
            }
        }

        /// @brief Inserts a new interval into the set using epsilon tolerance.
        ///
        /// This allows intervals that are separated by a gap smaller than or equal to
        /// `epsilon` to be merged into a single interval.
        ///
        /// @param new_interval The interval to insert.
        /// @param epsilon The tolerance for adjacency and intersection.
        BASALT_FORCE_INLINE void Insert(IntervalType new_interval, const ValueType epsilon)
        {
            if (new_interval.IsEmpty(epsilon))
            {
                return;
            }

            // Find strictly based on start - epsilon to catch fuzzy predecessors
            auto it = find_first_candidate(new_interval.GetStart() - epsilon);
            IntervalType merged_interval = new_interval;
            auto erase_start = it;

            // Merge loop uses epsilon logic
            while (it != m_intervals.end() && merged_interval.IntersectsOrAdjacent(*it, epsilon))
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
                    m_intervals.erase(erase_tail_begin, it);
                }
            }
            else
            {
                m_intervals.insert(it, merged_interval);
            }
        }

        /// @brief Erases an interval from the set.
        ///
        /// This operation will "subtract" the `interval_to_remove` from all
        /// intervals in the set, handling shrinking and splitting in-place.
        ///
        ///
        /// @param interval_to_remove The interval `[start, end)` to subtract.
        BASALT_FORCE_INLINE void Erase(IntervalType interval_to_remove)
        {
            if (interval_to_remove.IsEmpty())
            {
                return;
            }

            auto it = find_first_candidate(interval_to_remove.GetStart());
            if (it == m_intervals.end() || !interval_to_remove.Intersects(*it))
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
            while (erase_end != m_intervals.end() && interval_to_remove.Intersects(*erase_end))
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
                    m_intervals.insert(std::next(erase_start), *last_frag);
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
                    m_intervals.erase(erase_start);
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
                    m_intervals.erase(erase_tail_begin, erase_end);
                }

                if (first_frag.has_value() && last_frag.has_value())
                {
                    m_intervals.insert(std::next(erase_start), *last_frag);
                }
            }
            else
            {
                m_intervals.erase(erase_start, erase_end);
            }
        }

        /// @brief Erases an interval using epsilon tolerance for intersection checks.
        ///
        /// Note: The strict bounds of `interval_to_remove` are still used for the cut,
        /// but `epsilon` determines *which* intervals are considered touched/intersected.
        ///
        /// @param interval_to_remove The interval to subtract.
        /// @param epsilon The tolerance for checking intersections.
        BASALT_FORCE_INLINE void Erase(IntervalType interval_to_remove, const ValueType epsilon)
        {
            if (interval_to_remove.IsEmpty(epsilon))
            {
                return;
            }

            // Search slightly wider to account for epsilon overlaps
            auto it = find_first_candidate(interval_to_remove.GetStart() - epsilon);
            if (it == m_intervals.end() || !interval_to_remove.Intersects(*it, epsilon))
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
            while (erase_end != m_intervals.end() && interval_to_remove.Intersects(*erase_end, epsilon))
            {
                ++erase_end;
            }
            auto last_intersected_it = std::prev(erase_end);

            std::optional<IntervalType> last_frag;
            if (interval_to_remove.GetEnd() < last_intersected_it->GetEnd())
            {
                last_frag = IntervalType(interval_to_remove.GetEnd(), last_intersected_it->GetEnd());
            }

            // (Reuse logic from strict Erase for determining fragments/replacements)
            if (std::distance(erase_start, erase_end) == 1)
            {
                if (first_frag.has_value() && last_frag.has_value())
                {
                    *erase_start = *first_frag;
                    m_intervals.insert(std::next(erase_start), *last_frag);
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
                    m_intervals.erase(erase_start);
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
                    m_intervals.erase(erase_tail_begin, erase_end);
                }

                if (first_frag.has_value() && last_frag.has_value())
                {
                    m_intervals.insert(std::next(erase_start), *last_frag);
                }
            }
            else
            {
                m_intervals.erase(erase_start, erase_end);
            }
        }

        /// @brief Adds all intervals from another set to this set (set union).
        ///
        /// @param other The other IntervalSet to insert.
        BASALT_FORCE_INLINE void Insert(const IntervalSet& other)
        {
            if (other.IsEmpty())
            {
                return;
            }
            if (this->IsEmpty())
            {
                m_intervals = other.m_intervals;
                return;
            }

            container_type new_intervals;
            new_intervals.reserve(m_intervals.size() + other.m_intervals.size());

            auto it_this = m_intervals.begin();
            auto it_other = other.m_intervals.begin();

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

            while (it_this != m_intervals.end() || it_other != other.m_intervals.end())
            {
                const IntervalType* next_interval = nullptr;
                if (it_this == m_intervals.end() ||
                    (it_other != other.m_intervals.end() && it_other->GetStart() < it_this->GetStart()))
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
            m_intervals = std::move(new_intervals);
        }

        /// @brief Subtracts all intervals from another set from this set (set difference).
        ///
        /// @param other The other IntervalSet to subtract.
        BASALT_FORCE_INLINE void Erase(const IntervalSet& other)
        {
            if (other.IsEmpty() || this->IsEmpty())
            {
                return;
            }

            container_type new_intervals;
            new_intervals.reserve(m_intervals.size());

            auto it_this = m_intervals.begin();
            auto it_other = other.m_intervals.begin();

            std::optional<IntervalType> fragment;
            if (it_this != m_intervals.end())
            {
                fragment = *it_this;
                ++it_this;
            }

            while (fragment.has_value())
            {
                while (it_other != other.m_intervals.end() && it_other->GetEnd() <= fragment->GetStart())
                {
                    ++it_other;
                }

                if (it_other == other.m_intervals.end())
                {
                    new_intervals.push_back(*fragment);
                    new_intervals.insert(new_intervals.end(), it_this, m_intervals.end());
                    break;
                }

                if (it_other->GetStart() >= fragment->GetEnd())
                {
                    new_intervals.push_back(*fragment);
                    if (it_this != m_intervals.end())
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
                    if (it_this != m_intervals.end())
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

            m_intervals = std::move(new_intervals);
        }

        /// @brief Intersects this set with another set.
        ///
        /// Modifies this set to contain only the intervals that are present in both sets.
        ///
        /// @param other The other IntervalSet to intersect with.
        BASALT_FORCE_INLINE void Intersection(const IntervalSet& other)
        {
            container_type new_intervals;
            new_intervals.reserve(std::min(m_intervals.size(), other.m_intervals.size()));

            auto it1 = m_intervals.begin();
            auto it2 = other.m_intervals.begin();

            while (it1 != m_intervals.end() && it2 != other.m_intervals.end())
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
            m_intervals = std::move(new_intervals);
        }

        /// @brief Clips the set to the given boundary `[start, end)`.
        ///
        /// Removes all parts of the set that lie outside the boundary interval.
        ///
        /// @param boundary The interval defining the clipping region.
        BASALT_FORCE_INLINE void Clip(IntervalType boundary)
        {
            if (boundary.IsEmpty())
            {
                clear();
                return;
            }

            auto it_keep = find_first_candidate(boundary.GetStart());
            m_intervals.erase(m_intervals.begin(), it_keep);

            auto comp_start = [](const IntervalType& i, ValueType v)
            {
                return i.GetStart() < v;
            };
            auto it_erase = std::lower_bound(m_intervals.begin(), m_intervals.end(), boundary.GetEnd(), comp_start);
            m_intervals.erase(it_erase, m_intervals.end());

            if (m_intervals.empty())
            {
                return;
            }

            auto opt_i_first = m_intervals.front().Intersection(boundary);
            if (opt_i_first.has_value())
            {
                m_intervals.front() = *opt_i_first;
            }
            else
            {
                m_intervals.erase(m_intervals.begin());
            }

            if (m_intervals.empty())
            {
                return;
            }

            auto opt_i_last = m_intervals.back().Intersection(boundary);
            if (opt_i_last.has_value())
            {
                m_intervals.back() = *opt_i_last;
            }
            else
            {
                m_intervals.erase(std::prev(m_intervals.end()));
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
            if (candidate == m_intervals.end())
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
        [[nodiscard]] BASALT_FORCE_INLINE bool Contains(ValueType v, const ValueType epsilon) const noexcept
        {
            auto candidate = find_candidate_for_value(v);
            // If found, check strict/eps containment
            if (candidate != m_intervals.end())
            {
                if (candidate->Contains(v, epsilon))
                {
                    return true;
                }
            }
            // If not found (or close to boundaries), we must check surrounding intervals
            // because `v` might be in a gap slightly after `candidate` or slightly before `candidate+1`.
            // However, `find_candidate_for_value` finds the one *strictly* containing start.
            // With epsilon, we might fall into the interval *before* the one returned by upper_bound.

            // Check the interval strictly before the candidate as well
            // (find_candidate uses upper_bound then prev. If v is just past the end,
            // candidate is correct. If v is just before the start, candidate is the previous one.)
            auto it_check = find_first_candidate(v - epsilon);
            if (it_check != m_intervals.end() && it_check->Contains(v, epsilon))
            {
                return true;
            }

            return false;
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
            auto candidate = find_candidate_for_value(other.GetStart());
            if (candidate == m_intervals.end())
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
        [[nodiscard]] BASALT_FORCE_INLINE bool ContainsInterval(const IntervalType& other,
                                                                const ValueType epsilon) const noexcept
        {
            if (other.IsEmpty(epsilon))
            {
                return true;
            }
            // Look for an interval covering (start - eps)
            auto candidate = find_first_candidate(other.GetStart() - epsilon);
            if (candidate == m_intervals.end())
            {
                return false;
            }
            return candidate->ContainsInterval(other, epsilon);
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

            auto it = find_first_candidate(other.GetStart());
            if (it == m_intervals.end())
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

            auto it = find_first_candidate(other.GetStart() - epsilon);
            if (it == m_intervals.end())
            {
                return false;
            }

            return it->Intersects(other, epsilon);
        }

    private:
        container_type m_intervals;

        /// @brief Finds the first interval `it` such that `it.GetEnd() >= v`.
        ///
        /// @param v The value to search for.
        /// @return An iterator to the first candidate, or `end()`.
        BASALT_FORCE_INLINE iterator find_first_candidate(ValueType v) noexcept
        {
            constexpr auto comp = [](const IntervalType& i, ValueType value)
            {
                return i.GetEnd() < value;
            };
            return std::lower_bound(m_intervals.begin(), m_intervals.end(), v, comp);
        }

        /// @brief Finds the first interval `it` such that `it.GetEnd() >= v`.
        ///
        /// @param v The value to search for.
        /// @return A const_iterator to the first candidate, or `end()`.
        BASALT_FORCE_INLINE const_iterator find_first_candidate(ValueType v) const noexcept
        {
            constexpr auto comp = [](const IntervalType& i, ValueType value)
            {
                return i.GetEnd() < value;
            };
            return std::lower_bound(m_intervals.cbegin(), m_intervals.cend(), v, comp);
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
            auto it = std::upper_bound(m_intervals.begin(), m_intervals.end(), v, comp);
            if (it == m_intervals.begin())
            {
                return m_intervals.end();
            }
            return std::prev(it);
        }
    };

    /// @brief An implementation of IntervalSet using an `absl::btree_set`.
    ///
    /// This storage is optimized for frequent insertions and lookups in sets with a large number of disjoint intervals.
    ///
    /// @tparam T The type of the interval's endpoints.
    template <typename T>
    class IntervalSet<T, IntervalSetStorage::kBTree>
    {
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
            /// @brief Compares two intervals for strict partial ordering.
            ///
            /// @param a The left-hand side interval.
            /// @param b The right-hand side interval.
            /// @return \c true if `a` comes strictly before `b`.
            BASALT_FORCE_INLINE bool operator()(const IntervalType& a, const IntervalType& b) const noexcept
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
        /// @brief The size type.
        using size_type = container_type::size_type;

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

        /// @brief Returns a constant iterator to the beginning of the set.
        ///
        /// @return A const_iterator pointing to the first interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator begin() const noexcept
        {
            return m_intervals.begin();
        }

        /// @brief Returns a constant iterator to the end of the set.
        ///
        /// @return A const_iterator pointing one past the last interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator end() const noexcept
        {
            return m_intervals.end();
        }

        /// @brief Returns a constant iterator to the beginning of the set.
        ///
        /// @return A const_iterator pointing to the first interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator cbegin() const noexcept
        {
            return m_intervals.cbegin();
        }

        /// @brief Returns a constant iterator to the end of the set.
        ///
        /// @return A const_iterator pointing one past the last interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator cend() const noexcept
        {
            return m_intervals.cend();
        }

        /// @brief Checks if the set is empty.
        ///
        /// @return \c true if the set contains no intervals, \c false otherwise.
        [[nodiscard]] BASALT_FORCE_INLINE bool IsEmpty() const noexcept
        {
            return m_intervals.empty();
        }

        /// @brief Returns the number of disjoint intervals in the set.
        ///
        /// @return The number of intervals.
        [[nodiscard]] BASALT_FORCE_INLINE size_type size() const noexcept
        {
            return m_intervals.size();
        }

        /// @brief Clears all intervals from the set.
        BASALT_FORCE_INLINE void clear() noexcept
        {
            m_intervals.clear();
        }

        /// @brief Inserts a new interval into the set.
        ///
        /// The new interval will be merged with any existing intervals it overlaps or is adjacent to.
        ///
        /// @param new_interval The interval `[start, end)` to insert.
        BASALT_FORCE_INLINE void Insert(IntervalType new_interval)
        {
            if (new_interval.IsEmpty())
            {
                return;
            }

            auto it = find_first_candidate(new_interval.GetStart());
            IntervalType merged_interval = new_interval;
            auto erase_start = it;

            while (it != m_intervals.end() && merged_interval.IntersectsOrAdjacent(*it))
            {
                merged_interval = merged_interval.Combine(*it);
                ++it;
            }

            if (erase_start != it)
            {
                m_intervals.erase(erase_start, it);
            }

            m_intervals.insert(it, merged_interval);
        }

        /// @brief Inserts a new interval into the set using epsilon tolerance.
        ///
        /// This allows intervals that are separated by a gap smaller than or equal to
        /// `epsilon` to be merged into a single interval.
        ///
        /// @param new_interval The interval to insert.
        /// @param epsilon The tolerance for adjacency and intersection.
        BASALT_FORCE_INLINE void Insert(IntervalType new_interval, const ValueType epsilon)
        {
            if (new_interval.IsEmpty(epsilon))
            {
                return;
            }

            // Search for start - epsilon
            auto it = find_first_candidate(new_interval.GetStart() - epsilon);
            IntervalType merged_interval = new_interval;
            auto erase_start = it;

            // Merge based on epsilon connectivity
            while (it != m_intervals.end() && merged_interval.IntersectsOrAdjacent(*it, epsilon))
            {
                merged_interval = merged_interval.Combine(*it);
                ++it;
            }

            if (erase_start != it)
            {
                m_intervals.erase(erase_start, it);
            }

            m_intervals.insert(it, merged_interval);
        }

        /// @brief Adds all intervals from another set to this set (set union).
        ///
        /// @param other The other IntervalSet to insert.
        BASALT_FORCE_INLINE void Insert(const IntervalSet& other)
        {
            if (other.IsEmpty())
            {
                return;
            }
            if (this->IsEmpty())
            {
                m_intervals = other.m_intervals;
                return;
            }

            container_type new_intervals;
            auto it_this = m_intervals.begin();
            auto it_other = other.m_intervals.begin();

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

            while (it_this != m_intervals.end() || it_other != other.m_intervals.end())
            {
                const IntervalType* next_interval = nullptr;
                if (it_this == m_intervals.end() ||
                    (it_other != other.m_intervals.end() && it_other->GetStart() < it_this->GetStart()))
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
                    new_intervals.insert(*current_merged);
                    current_merged = *next_interval;
                }
            }

            new_intervals.insert(*current_merged);
            m_intervals = std::move(new_intervals);
        }

        /// @brief Erases an interval from the set.
        ///
        /// This operation will "subtract" the `interval_to_remove` from all
        /// intervals in the set, handling shrinking and splitting in-place.
        ///
        /// @param interval_to_remove The interval `[start, end)` to subtract.
        BASALT_FORCE_INLINE void Erase(IntervalType interval_to_remove)
        {
            if (interval_to_remove.IsEmpty())
            {
                return;
            }

            auto it = find_first_candidate(interval_to_remove.GetStart());

            if (it == m_intervals.end() || !interval_to_remove.Intersects(*it))
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
            while (erase_end != m_intervals.end() && interval_to_remove.Intersects(*erase_end))
            {
                ++erase_end;
            }
            auto last_intersected_it = std::prev(erase_end);

            std::optional<IntervalType> last_frag;
            if (interval_to_remove.GetEnd() < last_intersected_it->GetEnd())
            {
                last_frag = IntervalType(interval_to_remove.GetEnd(), last_intersected_it->GetEnd());
            }

            m_intervals.erase(erase_start, erase_end);

            if (first_frag.has_value())
            {
                m_intervals.insert(*first_frag);
            }
            if (last_frag.has_value())
            {
                m_intervals.insert(*last_frag);
            }
        }

        /// @brief Erases an interval using epsilon tolerance for intersection checks.
        ///
        /// Note: The strict bounds of `interval_to_remove` are still used for the cut,
        /// but `epsilon` determines *which* intervals are considered touched/intersected.
        ///
        /// @param interval_to_remove The interval to subtract.
        /// @param epsilon The tolerance for checking intersections.
        BASALT_FORCE_INLINE void Erase(IntervalType interval_to_remove, const ValueType epsilon)
        {
            if (interval_to_remove.IsEmpty(epsilon))
            {
                return;
            }

            auto it = find_first_candidate(interval_to_remove.GetStart() - epsilon);

            if (it == m_intervals.end() || !interval_to_remove.Intersects(*it, epsilon))
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
            while (erase_end != m_intervals.end() && interval_to_remove.Intersects(*erase_end, epsilon))
            {
                ++erase_end;
            }
            auto last_intersected_it = std::prev(erase_end);

            std::optional<IntervalType> last_frag;
            if (interval_to_remove.GetEnd() < last_intersected_it->GetEnd())
            {
                last_frag = IntervalType(interval_to_remove.GetEnd(), last_intersected_it->GetEnd());
            }

            m_intervals.erase(erase_start, erase_end);

            if (first_frag.has_value())
            {
                m_intervals.insert(*first_frag);
            }
            if (last_frag.has_value())
            {
                m_intervals.insert(*last_frag);
            }
        }

        /// @brief Subtracts all intervals from another set from this set (set difference).
        ///
        /// @param other The other IntervalSet to subtract.
        BASALT_FORCE_INLINE void Erase(const IntervalSet& other)
        {
            if (other.IsEmpty() || this->IsEmpty())
            {
                return;
            }

            container_type new_intervals;
            auto it_this = m_intervals.begin();
            auto it_other = other.m_intervals.begin();

            std::optional<IntervalType> fragment;
            if (it_this != m_intervals.end())
            {
                fragment = *it_this;
                ++it_this;
            }

            while (fragment.has_value())
            {
                while (it_other != other.m_intervals.end() && it_other->GetEnd() <= fragment->GetStart())
                {
                    ++it_other;
                }

                if (it_other == other.m_intervals.end())
                {
                    new_intervals.insert(*fragment);
                    new_intervals.insert(it_this, m_intervals.end());
                    break;
                }

                if (it_other->GetStart() >= fragment->GetEnd())
                {
                    new_intervals.insert(*fragment);
                    if (it_this != m_intervals.end())
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
                    new_intervals.insert(IntervalType(fragment->GetStart(), it_other->GetStart()));
                }

                if (fragment->GetEnd() > it_other->GetEnd())
                {
                    fragment = IntervalType(it_other->GetEnd(), fragment->GetEnd());
                    ++it_other;
                }
                else
                {
                    if (it_this != m_intervals.end())
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

            m_intervals = std::move(new_intervals);
        }

        /// @brief Intersects this set with another set.
        ///
        /// Modifies this set to contain only the intervals that are present in both sets.
        ///
        /// @param other The other IntervalSet to intersect with.
        BASALT_FORCE_INLINE void Intersection(const IntervalSet& other)
        {
            container_type new_intervals;

            auto it1 = m_intervals.begin();
            auto it2 = other.m_intervals.begin();
            std::optional<IntervalType> last_inserted;

            while (it1 != m_intervals.end() && it2 != other.m_intervals.end())
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
                            new_intervals.insert(*last_inserted);
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
                new_intervals.insert(*last_inserted);
            }

            m_intervals = std::move(new_intervals);
        }

        /// @brief Clips the set to the given boundary `[start, end)`.
        ///
        /// Removes all parts of the set that lie outside the boundary interval.
        ///
        /// @param boundary The interval defining the clipping region.
        BASALT_FORCE_INLINE void Clip(IntervalType boundary)
        {
            if (boundary.IsEmpty())
            {
                clear();
                return;
            }

            auto it_keep = find_first_candidate(boundary.GetStart());
            m_intervals.erase(m_intervals.begin(), it_keep);

            auto it_erase = m_intervals.begin();
            while (it_erase != m_intervals.end() && it_erase->GetStart() < boundary.GetEnd())
            {
                ++it_erase;
            }
            m_intervals.erase(it_erase, m_intervals.end());

            if (m_intervals.empty())
            {
                return;
            }

            auto first_node = m_intervals.extract(m_intervals.begin());
            if (auto opt_i = first_node.value().Intersection(boundary); opt_i.has_value())
            {
                first_node.value() = *opt_i;
                m_intervals.insert(std::move(first_node));
            }

            if (m_intervals.empty())
            {
                return;
            }

            auto last_node = m_intervals.extract(std::prev(m_intervals.end()));
            if (auto opt_i = last_node.value().Intersection(boundary); opt_i.has_value())
            {
                last_node.value() = *opt_i;
                m_intervals.insert(std::move(last_node));
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
            // the strict comparator. We use find_first_candidate to find the first interval
            // that ends *after* v.
            auto it = find_first_candidate(v);
            if (it == m_intervals.end())
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
            auto it = find_first_candidate(v - epsilon);
            if (it == m_intervals.end())
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
            // Find first interval that ends after other.Start
            auto it = find_first_candidate(other.GetStart());
            if (it == m_intervals.end())
            {
                return false;
            }
            // Check if it fully encloses 'other'
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
            auto it = find_first_candidate(other.GetStart() - epsilon);
            if (it == m_intervals.end())
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

            auto it = find_first_candidate(other.GetStart());
            if (it == m_intervals.end())
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

            auto it = find_first_candidate(other.GetStart() - epsilon);
            if (it == m_intervals.end())
            {
                return false;
            }

            return it->Intersects(other, epsilon);
        }

    private:
        /// @brief Finds the first interval `it` such that `it.GetEnd() > v`. (const)
        ///
        /// Uses the comparator `A.End <= B.Start`.
        /// `lower_bound(K)` returns the first `X` such that `comp(X, K)` is false.
        /// `X.End <= K.Start` is false => `X.End > K.Start`.
        /// By setting `K.Start` to `v`, we find the first interval where `End > v`.
        ///
        /// @param v The value to search for.
        /// @return A const iterator to the first candidate, or `end()`.
        BASALT_FORCE_INLINE const_iterator find_first_candidate(ValueType v) const noexcept
        {
            // We construct a dummy interval [v, v). The End doesn't matter for lower_bound
            // because the comparator uses the RHS Start.
            return m_intervals.lower_bound(IntervalType(v, v));
        }

        container_type m_intervals;
    };
}

#endif // BASALT_CONTAINER_INTERVAL_SET_H_
