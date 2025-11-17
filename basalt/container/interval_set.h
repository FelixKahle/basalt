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
#include "basalt/math/interval.h"

namespace bslt
{
    /// @brief Defines the underlying storage type for an IntervalSet.
    enum class IntervalSetStorage
    {
        kVector,
        kBTree,
    };

    /// @brief A set of disjoint, sorted intervals.
    ///
    /// This set is templated on the underlying value type (e.g., int, size_t)
    /// and is restricted to integral types. This restriction is necessary to
    /// allow for unambiguous conversion between different interval types
    /// (e.g., Closed, Open) and the canonical internal representation.
    ///
    /// All interval operations are handled by converting to a canonical
    /// internal representation: `ClosedOpenInterval<T>`, i.e., `[start, end)`.
    /// This type is chosen because it is "closed" under subtraction, meaning
    /// erasing one `[start, end)` interval from another always results in
    /// zero, one, or two new `[start, end)` intervals.
    ///
    /// @tparam T The integral type of the interval's endpoints.
    /// @tparam Storage The storage backend.
    template <typename T, IntervalSetStorage Storage = IntervalSetStorage::kVector>
        requires std::is_integral_v<T>
    class IntervalSet;

    /// @brief An implementation of IntervalSet using a `std::vector`.
    ///
    /// This implementation maintains a `std::vector` of `ClosedOpenInterval<T>`
    /// intervals that is always kept sorted and disjoint (merged).
    /// This invariant makes lookups and modifications efficient (O(log N) search,
    /// O(N) modification).
    template <typename T>
        requires std::is_integral_v<T>
    class IntervalSet<T, IntervalSetStorage::kVector>
    {
    public:
        /// @brief The underlying type of the interval endpoints (e.g., int, size_t).
        using ValueType = T;
        /// @brief The canonical interval type used for internal storage, `[start, end)`.
        using IntervalType = ClosedOpenInterval<ValueType>;
        /// @brief The underlying container type.
        using container_type = std::vector<IntervalType>;
        /// @brief A non-const iterator to the underlying container.
        using iterator = container_type::iterator;
        /// @brief A const iterator to the underlying container.
        using const_iterator = container_type::const_iterator;
        /// @brief The size type, typically `size_t`.
        using size_type = container_type::size_type;

        /// @brief Constructs an empty IntervalSet.
        constexpr BASALT_FORCE_INLINE IntervalSet() noexcept = default;

        /// @brief Constructs an empty IntervalSet with reserved capacity.
        /// @param capacity The number of intervals to reserve space for.
        explicit BASALT_FORCE_INLINE IntervalSet(const size_type capacity)
        {
            m_intervals.reserve(capacity);
        }

        /// @brief Returns a const iterator to the first interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator begin() const noexcept
        {
            return m_intervals.begin();
        }

        /// @brief Returns a const iterator following the last interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator end() const noexcept
        {
            return m_intervals.end();
        }

        /// @brief Returns a const iterator to the first interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator cbegin() const noexcept
        {
            return m_intervals.cbegin();
        }

        /// @brief Returns a const iterator following the last interval in the set.
        [[nodiscard]] BASALT_FORCE_INLINE const_iterator cend() const noexcept
        {
            return m_intervals.cend();
        }

        /// @brief Checks if the set is empty.
        /// @return \c true if the set contains no intervals, \c false otherwise.
        [[nodiscard]] BASALT_FORCE_INLINE bool IsEmpty() const noexcept
        {
            return m_intervals.empty();
        }

        /// @brief Gets the number of disjoint intervals in the set.
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

        /// @brief Inserts a new interval into the set (canonical).
        ///
        /// This is the canonical insertion operation. The new interval will
        /// be merged with any existing intervals it overlaps or is adjacent to.
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

        /// @brief Inserts a `[start, end]` (closed) interval.
        ///
        /// Converts `[start, end]` to the canonical `[start, end + 1)`.
        /// @param interval The interval to insert.
        BASALT_FORCE_INLINE void Insert(ClosedInterval<ValueType> interval)
        {
            if (interval.IsEmpty())
            {
                return;
            }
            Insert(IntervalType(interval.GetStart(), interval.GetEnd() + 1));
        }

        /// @brief Inserts a `(start, end)` (open) interval.
        ///
        /// Converts `(start, end)` to the canonical `[start + 1, end)`.
        /// @param interval The interval to insert.
        BASALT_FORCE_INLINE void Insert(OpenInterval<ValueType> interval)
        {
            if (interval.IsEmpty())
            {
                return;
            }
            Insert(IntervalType(interval.GetStart() + 1, interval.GetEnd()));
        }

        /// @brief Inserts a `(start, end]` (open-closed) interval.
        ///
        /// Converts `(start, end]` to the canonical `[start + 1, end + 1)`.
        /// @param interval The interval to insert.
        BASALT_FORCE_INLINE void Insert(OpenClosedInterval<ValueType> interval)
        {
            if (interval.IsEmpty())
            {
                return;
            }
            Insert(IntervalType(interval.GetStart() + 1, interval.GetEnd() + 1));
        }

        /// @brief Erases an interval from the set (canonical).
        ///
        /// This operation will "subtract" the `interval_to_remove` from all
        /// intervals in the set. Since the set uses `ClosedOpenInterval`,
        /// this operation is 100% correct and robust, efficiently handling
        /// shrinking, splitting, and deleting intervals in-place.
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

        /// @brief Erases a `[start, end]` (closed) interval.
        ///
        /// Converts `[start, end]` to the canonical `[start, end + 1)`.
        /// @param interval The interval to erase.
        BASALT_FORCE_INLINE void Erase(ClosedInterval<ValueType> interval)
        {
            Erase(IntervalType(interval.GetStart(), interval.GetEnd() + 1));
        }

        /// @brief Erases a `(start, end)` (open) interval.
        ///
        /// Converts `(start, end)` to the canonical `[start + 1, end)`.
        /// @param interval The interval to erase.
        BASALT_FORCE_INLINE void Erase(OpenInterval<ValueType> interval)
        {
            Erase(IntervalType(interval.GetStart() + 1, interval.GetEnd()));
        }

        /// @brief Erases a `(start, end]` (open-closed) interval.
        ///
        /// Converts `(start, end]` to the canonical `[start + 1, end + 1)`.
        /// @param interval The interval to erase.
        BASALT_FORCE_INLINE void Erase(OpenClosedInterval<ValueType> interval)
        {
            Erase(IntervalType(interval.GetStart() + 1, interval.GetEnd() + 1));
        }

        /// @brief Adds all intervals from another set to this set (set union).
        ///
        /// Iterates over the other set and inserts each of its intervals.
        /// The internal `Insert` logic handles merging.
        ///
        /// @param other The set to add.
        BASALT_FORCE_INLINE void Insert(const IntervalSet& other)
        {
            if (other.IsEmpty()) return;
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
        /// Iterates over the other set and erases each of its intervals.
        /// The internal `Erase` logic handles splitting/shrinking.
        ///
        /// @param other The set to subtract.
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
        /// Keeps only the intervals that are present in *both* sets.
        /// This is an efficient O(N+K) "merge" operation.
        ///
        /// @param other The set to intersect with.
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

        /// @brief Clips the set to the given boundary (canonical).
        ///
        /// Removes all parts of the set that are outside the boundary.
        /// This is an intersection with a single interval.
        ///
        /// @param boundary The boundary `[start, end)` to clip to.
        BASALT_FORCE_INLINE void Clip(IntervalType boundary)
        {
            if (boundary.IsEmpty())
            {
                clear();
                return;
            }

            container_type new_intervals;
            new_intervals.reserve(m_intervals.size());

            auto it = find_first_candidate(boundary.GetStart());

            while (it != m_intervals.end() && boundary.Intersects(*it))
            {
                if (auto opt_i = boundary.Intersection(*it); opt_i.has_value())
                {
                    new_intervals.push_back(opt_i.value());
                }
                ++it;
            }
            m_intervals = std::move(new_intervals);
        }

        /// @brief Clips the set to a `[start, end]` (closed) boundary.
        BASALT_FORCE_INLINE void Clip(ClosedInterval<ValueType> boundary)
        {
            if (boundary.IsEmpty())
            {
                clear();
                return;
            }
            Clip(IntervalType(boundary.GetStart(), boundary.GetEnd() + 1));
        }

        /// @brief Clips the set to a `(start, end)` (open) boundary.
        BASALT_FORCE_INLINE void Clip(OpenInterval<ValueType> boundary)
        {
            if (boundary.IsEmpty())
            {
                clear();
                return;
            }
            Clip(IntervalType(boundary.GetStart() + 1, boundary.GetEnd()));
        }

        /// @brief Clips the set to a `(start, end]` (open-closed) boundary.
        BASALT_FORCE_INLINE void Clip(OpenClosedInterval<ValueType> boundary)
        {
            if (boundary.IsEmpty())
            {
                clear();
                return;
            }
            Clip(IntervalType(boundary.GetStart() + 1, boundary.GetEnd() + 1));
        }

        /// @brief Set union (add).
        BASALT_FORCE_INLINE IntervalSet& operator+=(const IntervalSet& other)
        {
            Insert(other);
            return *this;
        }

        /// @brief Set difference (subtract).
        BASALT_FORCE_INLINE IntervalSet& operator-=(const IntervalSet& other)
        {
            Erase(other);
            return *this;
        }

        /// @brief Set intersection.
        BASALT_FORCE_INLINE IntervalSet& operator&=(const IntervalSet& other)
        {
            Intersection(other);
            return *this;
        }

        /// @brief Set union (add).
        friend BASALT_FORCE_INLINE IntervalSet operator+(IntervalSet lhs, const IntervalSet& rhs)
        {
            lhs += rhs;
            return lhs;
        }

        /// @brief Set difference (subtract).
        friend BASALT_FORCE_INLINE IntervalSet operator-(IntervalSet lhs, const IntervalSet& rhs)
        {
            lhs -= rhs;
            return lhs;
        }

        /// @brief Set intersection.
        friend BASALT_FORCE_INLINE IntervalSet operator&(IntervalSet lhs, const IntervalSet& rhs)
        {
            lhs &= rhs;
            return lhs;
        }

        /// @brief Checks if a specific value is contained in the set.
        /// @param v The value to check.
        /// @return \c true if any interval in the set contains \p v.
        [[nodiscard]] BASALT_FORCE_INLINE bool Contains(ValueType v) const noexcept
        {
            auto candidate = find_candidate_for_value(v);
            if (candidate == m_intervals.end())
            {
                return false;
            }
            return v < candidate->GetEnd();
        }

        /// @brief Checks if an interval is fully contained in the set (canonical).
        ///
        /// The `other` interval must be fully contained within a *single*
        /// existing interval in the set.
        ///
        /// @param other The interval `[start, end)` to check.
        /// @return \c true if \p other is fully contained in the set.
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

        /// @brief Checks if a `[start, end]` interval is contained.
        [[nodiscard]] BASALT_FORCE_INLINE bool ContainsInterval(ClosedInterval<ValueType> interval) const noexcept
        {
            if (interval.IsEmpty())
            {
                return true;
            }
            return ContainsInterval(IntervalType(interval.GetStart(), interval.GetEnd() + 1));
        }

        /// @brief Checks if a `(start, end)` interval is contained.
        [[nodiscard]] BASALT_FORCE_INLINE bool ContainsInterval(OpenInterval<ValueType> interval) const noexcept
        {
            if (interval.IsEmpty())
            {
                return true;
            }
            return ContainsInterval(IntervalType(interval.GetStart() + 1, interval.GetEnd()));
        }

        /// @brief Checks if a `(start, end]` interval is contained.
        [[nodiscard]] BASALT_FORCE_INLINE bool ContainsInterval(OpenClosedInterval<ValueType> interval) const noexcept
        {
            if (interval.IsEmpty())
            {
                return true;
            }
            return ContainsInterval(IntervalType(interval.GetStart() + 1, interval.GetEnd() + 1));
        }

        /// @brief Checks if the set has any overlap with a given interval (canonical).
        /// @param other The interval `[start, end)` to check for intersection.
        /// @return \c true if \p other intersects any interval in the set.
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

        /// @brief Checks if the set intersects a `[start, end]` interval.
        [[nodiscard]] BASALT_FORCE_INLINE bool Intersects(ClosedInterval<ValueType> interval) const noexcept
        {
            if (interval.IsEmpty())
            {
                return false;
            }
            return Intersects(IntervalType(interval.GetStart(), interval.GetEnd() + 1));
        }

        /// @brief Checks if the set intersects a `(start, end)` interval.
        [[nodiscard]] BASALT_FORCE_INLINE bool Intersects(OpenInterval<ValueType> interval) const noexcept
        {
            if (interval.IsEmpty())
            {
                return false;
            }
            return Intersects(IntervalType(interval.GetStart() + 1, interval.GetEnd()));
        }

        /// @brief Checks if the set intersects a `(start, end]` interval.
        [[nodiscard]] BASALT_FORCE_INLINE bool Intersects(OpenClosedInterval<ValueType> interval) const noexcept
        {
            if (interval.IsEmpty())
            {
                return false;
            }
            return Intersects(IntervalType(interval.GetStart() + 1, interval.GetEnd() + 1));
        }

    private:
        /// @brief The sorted, disjoint set of intervals.
        container_type m_intervals;

        /// @brief Finds the first interval `it` such that `it.GetEnd() >= v`. (non-const)
        ///
        /// This is the first *potential* interval that could intersect
        /// or be adjacent to a new interval starting at `v`.
        ///
        /// @param v The value (e.g., a new interval's start).
        /// @return An iterator to the first candidate, or `end()`.
        BASALT_FORCE_INLINE iterator find_first_candidate(ValueType v) noexcept
        {
            constexpr auto comp = [](const IntervalType& i, ValueType value)
            {
                return i.GetEnd() < value;
            };
            return std::lower_bound(m_intervals.begin(), m_intervals.end(), v, comp);
        }

        /// @brief Finds the first interval `it` such that `it.GetEnd() >= v`. (const)
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
        /// This uses `upper_bound` to find the first interval that *starts*
        /// after `v`, and then returns the interval immediately before it.
        /// That previous interval is the only one that could possibly contain `v`.
        ///
        /// @param v The value to check.
        /// @return A const iterator to the candidate, or `end()` if no candidate exists.
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

    template <typename T>
        requires std::is_integral_v<T>
    class IntervalSet<T, IntervalSetStorage::kBTree>
    {
        // ReSharper disable once CppStaticAssertFailure
        static_assert(false, "IntervalSetStorage::kBTree is not yet implemented.");
    };
}

#endif // BASALT_CONTAINER_INTERVAL_SET_H_
