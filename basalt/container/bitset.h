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

#ifndef BASALT_CONTAINER_BITSET_H_
#define BASALT_CONTAINER_BITSET_H_

#include <algorithm>
#include <cassert>
#include <cstring>
#include <string>
#include <vector>
#include <limits>
#include <type_traits>
#include <iterator>
#include <compare>
#include <ostream>
#include "absl/log/check.h"
#include "absl/strings/str_format.h"
#include "basalt/base/config.h"
#include "basalt/utils/bits.h"
#include "basalt/type_traits/type_traits.h"

namespace bslt
{
    /// @brief A high-performance dynamic bitset implementation.
    ///
    /// Optimizes for word-level parallelism, CPU intrinsics (via bits.h), and
    /// contiguous memory layout. Supports fast iteration and block shifting.
    /// Fully compatible with C++ STL iterators and C++20 Ranges.
    ///
    /// @tparam S The underlying storage type (must be an unsigned integer, default uint64_t).
    /// @tparam T The size type (default std::size_t).
    /// @tparam Alloc The allocator for the underlying vector (default std::allocator).
    template <typename S = uint64_t, typename T = std::size_t, typename Alloc = std::allocator<S>>
        requires std::is_unsigned_v<S>
    class Bitset
    {
    public:
        using StorageType = S;
        using SizeType = T;
        using AllocatorType = Alloc;

        // Forward declaration
        template <bool IsConst>
        class BitIterator;

        using iterator = BitIterator<false>;
        using const_iterator = BitIterator<true>;
        using reverse_iterator = std::reverse_iterator<iterator>;
        using const_reverse_iterator = std::reverse_iterator<const_iterator>;

        /// @brief Proxy class for array-style access (bitset[i] = true).
        ///
        /// Provides a reference-like interface to a single bit within the bitset,
        /// allowing for read and write operations.
        class Reference
        {
        public:
            /// @brief Default copy constructor.
            Reference(const Reference&) noexcept = default;

            /// @brief Default destructor.
            ~Reference() noexcept = default;

            /// @brief Assigns a boolean value to the referenced bit.
            ///
            /// Modifies the underlying bitset at the specific index.
            ///
            /// @param value The value to set (true for 1, false for 0).
            /// @return A reference to this proxy object.
            BASALT_FORCE_INLINE Reference& operator=(const bool value) noexcept
            {
                if (value)
                {
                    parent_.Set(index_);
                }
                else
                {
                    parent_.Clear(index_);
                }
                return *this;
            }

            /// @brief Assigns the value of another reference to this bit.
            ///
            /// @param other The other reference to copy from.
            /// @return A reference to this proxy object.
            BASALT_FORCE_INLINE Reference& operator=(const Reference& other) noexcept
            {
                *this = static_cast<bool>(other);
                return *this;
            }

            /// @brief Implicit conversion to boolean.
            ///
            /// Allows the reference to be used in boolean contexts (e.g., if (bit)).
            ///
            /// @return True if the bit is set, false otherwise.
            BASALT_FORCE_INLINE operator bool() const noexcept // NOLINT(*-explicit-constructor)
            {
                return parent_.Test(index_);
            }

            /// @brief Swaps the values of two references.
            ///
            /// @param lhs The first reference.
            /// @param rhs The second reference.
            friend BASALT_FORCE_INLINE void swap(const Reference lhs, const Reference rhs) noexcept
            {
                const bool x = static_cast<bool>(lhs);
                const bool y = static_cast<bool>(rhs);
                lhs.parent_.Set(lhs.index_, y);
                rhs.parent_.Set(rhs.index_, x);
            }

        private:
            friend class Bitset;
            friend class BitIterator<false>;

            Bitset& parent_;
            SizeType index_;

            BASALT_FORCE_INLINE Reference(Bitset& parent, SizeType index) noexcept
                : parent_(parent),
                  index_(index)
            {
            }
        };

        /// @brief Random Access Iterator implementation for STL/Ranges compatibility.
        ///
        /// Enables usage of the bitset with standard algorithms and range-based loops.
        ///
        /// @tparam IsConst Boolean flag indicating if this is a const iterator.
        template <bool IsConst>
        class BitIterator
        {
        public:
            using iterator_category = std::random_access_iterator_tag;
            using iterator_concept = std::random_access_iterator_tag;
            using value_type = bool;
            using difference_type = std::ptrdiff_t;
            using pointer = void;
            using reference = std::conditional_t<IsConst, bool, Reference>;
            using ParentPtr = std::conditional_t<IsConst, const Bitset*, Bitset*>;

            /// @brief Default constructor.
            BASALT_FORCE_INLINE BitIterator() noexcept
                : parent_(nullptr),
                  index_(0)
            {
            }

            /// @brief Constructs an iterator for a specific bitset and index.
            ///
            /// @param parent Pointer to the parent bitset.
            /// @param index The starting index.
            BASALT_FORCE_INLINE BitIterator(ParentPtr parent, SizeType index) noexcept
                : parent_(parent),
                  index_(index)
            {
            }

            /// @brief Copy constructor allowing conversion from non-const to const iterator.
            ///
            /// @tparam WasConst Flag of the source iterator.
            /// @param other The source iterator.
            template <bool WasConst>
                requires IsConst && (!WasConst)
            explicit BASALT_FORCE_INLINE BitIterator(const BitIterator<WasConst>& other) noexcept
                : parent_(other.parent_),
                  index_(other.index_)
            {
            }

            /// @brief Dereferences the iterator.
            ///
            /// @return The value of the bit (bool for const, Reference for non-const).
            BASALT_FORCE_INLINE reference operator*() const noexcept
            {
                if constexpr (IsConst)
                {
                    return parent_->Test(index_);
                }
                else
                {
                    return Reference(*parent_, index_);
                }
            }

            /// @brief Accesses the element at a specific offset.
            ///
            /// @param n The offset.
            /// @return The value of the bit at the offset.
            BASALT_FORCE_INLINE reference operator[](difference_type n) const noexcept
            {
                return *(*this + n);
            }

            /// @brief Pre-increment operator.
            ///
            /// @return Reference to the incremented iterator.
            BASALT_FORCE_INLINE BitIterator& operator++() noexcept
            {
                ++index_;
                return *this;
            }

            /// @brief Post-increment operator.
            ///
            /// @return The iterator value before incrementing.
            BASALT_FORCE_INLINE BitIterator operator++(int) noexcept
            {
                BitIterator temp = *this;
                ++index_;
                return temp;
            }

            /// @brief Pre-decrement operator.
            ///
            /// @return Reference to the decremented iterator.
            BASALT_FORCE_INLINE BitIterator& operator--() noexcept
            {
                --index_;
                return *this;
            }

            /// @brief Post-decrement operator.
            ///
            /// @return The iterator value before decrementing.
            BASALT_FORCE_INLINE BitIterator operator--(int) noexcept
            {
                BitIterator temp = *this;
                --index_;
                return temp;
            }

            /// @brief Advances the iterator by n positions.
            ///
            /// @param n The number of positions to advance.
            /// @return Reference to the advanced iterator.
            BASALT_FORCE_INLINE BitIterator& operator+=(difference_type n) noexcept
            {
                index_ += n;
                return *this;
            }

            /// @brief Retreats the iterator by n positions.
            ///
            /// @param n The number of positions to retreat.
            /// @return Reference to the retreated iterator.
            BASALT_FORCE_INLINE BitIterator& operator-=(difference_type n) noexcept
            {
                index_ -= n;
                return *this;
            }

            /// @brief Adds an offset to an iterator.
            ///
            /// @param it The iterator.
            /// @param n The offset.
            /// @return The new iterator.
            BASALT_FORCE_INLINE friend BitIterator operator+(BitIterator it, difference_type n) noexcept
            {
                it += n;
                return it;
            }

            /// @brief Adds an iterator to an offset.
            ///
            /// @param n The offset.
            /// @param it The iterator.
            /// @return The new iterator.
            BASALT_FORCE_INLINE friend BitIterator operator+(difference_type n, BitIterator it) noexcept
            {
                it += n;
                return it;
            }

            /// @brief Subtracts an offset from an iterator.
            ///
            /// @param it The iterator.
            /// @param n The offset.
            /// @return The new iterator.
            BASALT_FORCE_INLINE friend BitIterator operator-(BitIterator it, difference_type n) noexcept
            {
                it -= n;
                return it;
            }

            /// @brief Computes the distance between two iterators.
            ///
            /// @param lhs The first iterator.
            /// @param rhs The second iterator.
            /// @return The distance (lhs - rhs).
            BASALT_FORCE_INLINE friend difference_type operator-(const BitIterator& lhs,
                                                                 const BitIterator& rhs) noexcept
            {
                DCHECK_EQ(lhs.parent_, rhs.parent_);

                return static_cast<difference_type>(lhs.index_) - static_cast<difference_type>(rhs.index_);
            }

            /// @brief Checks if two iterators are equal.
            ///
            /// @param lhs The first iterator.
            /// @param rhs The second iterator.
            /// @return True if they point to the same bit in the same bitset.
            BASALT_FORCE_INLINE friend bool operator==(const BitIterator& lhs, const BitIterator& rhs) noexcept
            {
                return lhs.parent_ == rhs.parent_ && lhs.index_ == rhs.index_;
            }

            /// @brief Compares two iterators.
            ///
            /// @param lhs The first iterator.
            /// @param rhs The second iterator.
            /// @return The ordering relation.
            BASALT_FORCE_INLINE friend std::strong_ordering operator<=>(
                const BitIterator& lhs, const BitIterator& rhs) noexcept
            {
                DCHECK_EQ(lhs.parent_, rhs.parent_);

                return lhs.index_ <=> rhs.index_;
            }

        private:
            friend class Bitset;
            template <bool>
            friend class BitIterator;

            ParentPtr parent_;
            SizeType index_;
        };

        /// @brief Default constructs an empty bitset.
        BASALT_FORCE_INLINE Bitset() noexcept = default;

        /// @brief Constructs a bitset with a specific size.
        ///
        /// @param size The number of bits.
        /// @param value The initial value for all bits (default false).
        /// @param alloc The allocator to use.
        explicit BASALT_FORCE_INLINE Bitset(SizeType size, const bool value = false,
                                            const Alloc& alloc = Alloc())
            : data_(alloc),
              num_bits_(size)
        {
            const SizeType num_words = bits::BitLength<StorageType>(size);
            if (value)
            {
                data_.assign(num_words, std::numeric_limits<StorageType>::max());
                ClearUnusedBits();
            }
            else
            {
                data_.assign(num_words, 0);
            }
        }

        /// @brief Returns an iterator to the first bit.
        ///
        /// @return An iterator to the beginning.
        BASALT_FORCE_INLINE iterator begin() noexcept
        {
            return iterator(this, 0);
        }

        /// @brief Returns an iterator to the element following the last bit.
        ///
        /// @return An iterator to the end.
        BASALT_FORCE_INLINE iterator end() noexcept
        {
            return iterator(this, num_bits_);
        }

        /// @brief Returns a const iterator to the first bit.
        ///
        /// @return A const iterator to the beginning.
        BASALT_FORCE_INLINE const_iterator begin() const noexcept
        {
            return const_iterator(this, 0);
        }

        /// @brief Returns a const iterator to the element following the last bit.
        ///
        /// @return A const iterator to the end.
        BASALT_FORCE_INLINE const_iterator end() const noexcept
        {
            return const_iterator(this, num_bits_);
        }

        /// @brief Returns a const iterator to the first bit.
        ///
        /// @return A const iterator to the beginning.
        BASALT_FORCE_INLINE const_iterator cbegin() const noexcept
        {
            return const_iterator(this, 0);
        }

        /// @brief Returns a const iterator to the element following the last bit.
        ///
        /// @return A const iterator to the end.
        BASALT_FORCE_INLINE const_iterator cend() const noexcept
        {
            return const_iterator(this, num_bits_);
        }

        /// @brief Returns a reverse iterator to the first bit of the reversed bitset.
        ///
        /// @return A reverse iterator to the beginning.
        BASALT_FORCE_INLINE reverse_iterator rbegin() noexcept
        {
            return std::make_reverse_iterator(end());
        }

        /// @brief Returns a reverse iterator to the element following the last bit of the reversed bitset.
        ///
        /// @return A reverse iterator to the end.
        BASALT_FORCE_INLINE reverse_iterator rend() noexcept
        {
            return std::make_reverse_iterator(begin());
        }

        /// @brief Returns a const reverse iterator to the first bit of the reversed bitset.
        ///
        /// @return A const reverse iterator to the beginning.
        BASALT_FORCE_INLINE const_reverse_iterator crbegin() const noexcept
        {
            return std::make_reverse_iterator(cend());
        }

        /// @brief Returns a const reverse iterator to the element following the last bit of the reversed bitset.
        ///
        /// @return A const reverse iterator to the end.
        BASALT_FORCE_INLINE const_reverse_iterator crend() const noexcept
        {
            return std::make_reverse_iterator(cbegin());
        }

        /// @brief Returns the number of bits in the bitset.
        ///
        /// @return The size of the bitset.
        [[nodiscard]] BASALT_FORCE_INLINE SizeType size() const noexcept
        {
            return num_bits_;
        }

        /// @brief Checks if the bitset is empty.
        ///
        /// @return True if the size is 0, false otherwise.
        [[nodiscard]] BASALT_FORCE_INLINE bool empty() const noexcept
        {
            return num_bits_ == 0;
        }

        /// @brief Returns the total number of bits that the bitset can hold without reallocation.
        ///
        /// @return The capacity in bits.
        [[nodiscard]] BASALT_FORCE_INLINE SizeType capacity() const noexcept
        {
            return data_.capacity() * kBitsPerWord;
        }

        /// @brief Resizes the bitset to contain the specified number of bits.
        ///
        /// If the new size is greater than the current size, new bits are initialized to `value`.
        ///
        /// @param new_size The new size of the bitset.
        /// @param value The value to initialize new bits with (default false).
        void Resize(SizeType new_size, bool value = false)
        {
            if (new_size == num_bits_)
            {
                return;
            }

            const SizeType old_num_words = data_.size();
            const SizeType new_num_words = bits::BitLength<StorageType>(new_size);

            if (new_size < num_bits_)
            {
                data_.resize(new_num_words);
                num_bits_ = new_size;
                ClearUnusedBits();
            }
            else
            {
                if (new_num_words > old_num_words)
                {
                    StorageType fill_pattern = value ? std::numeric_limits<StorageType>::max() : 0;
                    data_.resize(new_num_words, fill_pattern);
                }

                if (value && num_bits_ % kBitsPerWord != 0)
                {
                    SizeType old_last_word_idx = old_num_words - 1;
                    SizeType bits_in_old_last = num_bits_ % kBitsPerWord;
                    StorageType mask = bits::IntervalUp<StorageType>(static_cast<StorageType>(bits_in_old_last));
                    data_[old_last_word_idx] |= mask;
                }

                num_bits_ = new_size;
                ClearUnusedBits();
            }
        }

        /// @brief Clears the bitset, removing all bits.
        BASALT_FORCE_INLINE void Clear() noexcept
        {
            num_bits_ = 0;
            data_.clear();
        }

        /// @brief Requests that the bitset capacity be at least enough to contain n bits.
        ///
        /// @param num_bits The minimum number of bits to reserve space for.
        BASALT_FORCE_INLINE void Reserve(SizeType num_bits)
        {
            data_.reserve(bits::BitLength<StorageType>(num_bits));
        }

        /// @brief Tests whether the bit at position pos is set.
        ///
        /// @param pos The index of the bit.
        /// @return True if the bit is set, false otherwise.
        [[nodiscard]] BASALT_FORCE_INLINE bool Test(SizeType pos) const noexcept
        {
            DCHECK_LT(pos, num_bits_);
            return bits::IsBitSet(data_.data(), pos);
        }

        /// @brief Accesses the bit at position pos (read-only).
        ///
        /// @param pos The index of the bit.
        /// @return True if the bit is set, false otherwise.
        BASALT_FORCE_INLINE bool operator[](SizeType pos) const noexcept
        {
            return Test(pos);
        }

        /// @brief Accesses the bit at position pos (read-write).
        ///
        /// @param pos The index of the bit.
        /// @return A Reference proxy to the bit.
        BASALT_FORCE_INLINE Reference operator[](SizeType pos) noexcept
        {
            DCHECK_LT(pos, num_bits_);
            return Reference(*this, pos);
        }

        /// @brief Sets the bit at position pos to true.
        ///
        /// @param pos The index of the bit.
        BASALT_FORCE_INLINE void Set(SizeType pos) noexcept
        {
            DCHECK_LT(pos, num_bits_);
            bits::SetBit(data_.data(), pos);
        }

        /// @brief Sets the bit at position pos to false.
        ///
        /// @param pos The index of the bit.
        BASALT_FORCE_INLINE void Clear(SizeType pos) noexcept
        {
            DCHECK_LT(pos, num_bits_);
            bits::ClearBit(data_.data(), pos);
        }

        /// @brief Sets the bit at position pos to the specified value.
        ///
        /// @param pos The index of the bit.
        /// @param value The value to set.
        BASALT_FORCE_INLINE void Set(SizeType pos, const bool value) noexcept
        {
            if (value)
            {
                Set(pos);
            }
            else
            {
                Clear(pos);
            }
        }

        /// @brief Toggles the bit at position pos.
        ///
        /// @param pos The index of the bit.
        BASALT_FORCE_INLINE void Flip(SizeType pos) noexcept
        {
            DCHECK_LT(pos, num_bits_);
            data_[bits::BitOffset<StorageType>(pos)] ^= bits::BitMask<StorageType>(bits::BitPosition<StorageType>(pos));
        }

        /// @brief Sets all bits to true.
        BASALT_FORCE_INLINE void SetAll() noexcept
        {
            if (empty())
            {
                return;
            }
            std::memset(data_.data(), 0xFF, data_.size() * sizeof(StorageType));
            ClearUnusedBits();
        }

        /// @brief Sets all bits to false.
        BASALT_FORCE_INLINE void ResetAll() noexcept
        {
            if (empty())
            {
                return;
            }
            std::memset(data_.data(), 0, data_.size() * sizeof(StorageType));
        }

        /// @brief Toggles all bits.
        BASALT_FORCE_INLINE void FlipAll() noexcept
        {
            for (auto& word : data_)
            {
                word = ~word;
            }
            ClearUnusedBits();
        }

        /// @brief Counts the number of set bits.
        ///
        /// @return The number of bits set to true.
        [[nodiscard]] BASALT_FORCE_INLINE SizeType Count() const noexcept
        {
            SizeType count = 0;
            for (const auto& word : data_)
            {
                count += bits::BitCount<StorageType>(word);
            }
            return count;
        }

        /// @brief Checks if any bit is set to true.
        ///
        /// @return True if at least one bit is set, false otherwise.
        [[nodiscard]] BASALT_FORCE_INLINE bool Any() const noexcept
        {
            for (const auto& word : data_)
            {
                if (word != 0)
                {
                    return true;
                }
            }
            return false;
        }

        /// @brief Checks if no bits are set to true.
        ///
        /// @return True if all bits are false, false otherwise.
        [[nodiscard]] BASALT_FORCE_INLINE bool None() const noexcept
        {
            return !Any();
        }

        /// @brief Checks if all bits are set to true.
        ///
        /// @return True if all bits are true, false otherwise.
        [[nodiscard]] BASALT_FORCE_INLINE bool All() const noexcept
        {
            if (empty())
            {
                return true;
            }

            const SizeType full_words = num_bits_ / kBitsPerWord;
            for (SizeType i = 0; i < full_words; ++i)
            {
                // ReSharper disable once CppDFANullDereference
                if (data_[i] != std::numeric_limits<StorageType>::max())
                {
                    return false;
                }
            }

            if (num_bits_ % kBitsPerWord != 0)
            {
                StorageType mask = (StorageType{1} << (num_bits_ % kBitsPerWord)) - 1;
                if (data_.back() != mask)
                {
                    return false;
                }
            }
            return true;
        }

        /// @brief Finds the index of the first set bit.
        ///
        /// @return The index of the first set bit, or -1 if no bits are set.
        [[nodiscard]] BASALT_FORCE_INLINE int64_t FindFirst() const noexcept
        {
            if (empty())
            {
                return -1;
            }
            return bits::LeastSignificantBitPosition<StorageType>(data_.data(), 0, num_bits_ - 1);
        }

        /// @brief Finds the index of the next set bit after a given position.
        ///
        /// @param prev The position to start searching after.
        /// @return The index of the next set bit, or -1 if none are found.
        [[nodiscard]] int64_t FindNext(SizeType prev) const noexcept
        {
            if (empty() || prev >= num_bits_ - 1)
            {
                return -1;
            }
            return bits::LeastSignificantBitPosition<StorageType>(data_.data(), prev + 1, num_bits_ - 1);
        }

        /// @brief Finds the index of the first unset bit (first bit that is false).
        ///
        /// @return The index of the first unset bit, or -1 if all bits are set.
        [[nodiscard]] int64_t FindFirstUnset() const noexcept
        {
            for (SizeType i = 0; i < data_.size(); ++i)
            {
                // ReSharper disable once CppDFANullDereference
                if (data_[i] == std::numeric_limits<StorageType>::max())
                {
                    continue;
                }

                StorageType inverted = ~data_[i];

                if (i == data_.size() - 1)
                {
                    const SizeType extra = num_bits_ % kBitsPerWord;
                    if (extra != 0)
                    {
                        inverted &= (StorageType{1} << extra) - 1;
                    }
                }

                if (inverted != 0)
                {
                    return static_cast<int64_t>(i * kBitsPerWord + bits::LeastSignificantBitPosition<StorageType>(
                        inverted));
                }
            }
            return -1;
        }

        /// @brief Efficiently iterates over every set bit using a callback.
        ///
        /// This method avoids checking every bit by skipping zero words and
        /// iterating only over set bits within words.
        ///
        /// @tparam Func The type of the callback function.
        /// @param func The callback to invoke for each set bit index.
        template <typename Func>
        void ForEachSetBit(Func&& func) const
        {
            for (SizeType i = 0; i < data_.size(); ++i)
            {
                // ReSharper disable once CppDFANullDereference
                StorageType word = data_[i];
                while (word != 0)
                {
                    // Get local LSB index
                    StorageType lsb = bits::LeastSignificantBitPosition<StorageType>(word);
                    // Execute callback
                    func(static_cast<SizeType>(i * kBitsPerWord + lsb));
                    // Clear the LSB (x & (x-1)) - Standard compiler optimization pattern
                    word &= (word - 1);
                }
            }
        }

        /// @brief Performs a bitwise AND assignment with another bitset.
        ///
        /// @param rhs The right-hand side bitset.
        /// @return A reference to this bitset.
        BASALT_FORCE_INLINE Bitset& operator&=(const Bitset& rhs) noexcept
        {
            DCHECK_EQ(num_bits_, rhs.num_bits_);
            for (SizeType i = 0; i < data_.size(); ++i)
            {
                // ReSharper disable once CppDFANullDereference
                data_[i] &= rhs.data_[i];
            }
            return *this;
        }

        /// @brief Performs a bitwise OR assignment with another bitset.
        ///
        /// @param rhs The right-hand side bitset.
        /// @return A reference to this bitset.
        BASALT_FORCE_INLINE Bitset& operator|=(const Bitset& rhs) noexcept
        {
            DCHECK_EQ(num_bits_, rhs.num_bits_);
            for (SizeType i = 0; i < data_.size(); ++i)
            {
                // ReSharper disable once CppDFANullDereference
                data_[i] |= rhs.data_[i];
            }
            return *this;
        }

        /// @brief Performs a bitwise XOR assignment with another bitset.
        ///
        /// @param rhs The right-hand side bitset.
        /// @return A reference to this bitset.
        BASALT_FORCE_INLINE Bitset& operator^=(const Bitset& rhs) noexcept
        {
            DCHECK_EQ(num_bits_, rhs.num_bits_);
            for (SizeType i = 0; i < data_.size(); ++i)
            {
                // ReSharper disable once CppDFANullDereference
                data_[i] ^= rhs.data_[i];
            }
            return *this;
        }

        /// @brief Returns a new bitset that is the bitwise NOT of this bitset.
        ///
        /// @return A new inverted bitset.
        BASALT_FORCE_INLINE Bitset operator~() const
        {
            Bitset copy = *this;
            copy.FlipAll();
            return copy;
        }

        /// @brief Shifts all bits left by count positions (efficient word-level shift).
        ///
        /// @param count The number of positions to shift.
        /// @return A reference to this bitset.
        Bitset& operator<<=(SizeType count) noexcept
        {
            if (count == 0 || empty())
            {
                return *this;
            }
            if (count >= num_bits_)
            {
                ResetAll();
                return *this;
            }

            const SizeType word_shift = count / kBitsPerWord;
            const SizeType bit_shift = count % kBitsPerWord;
            const SizeType num_words = data_.size();

            if (word_shift > 0)
            {
                std::memmove(data_.data() + word_shift, data_.data(), (num_words - word_shift) * sizeof(StorageType));
                std::memset(data_.data(), 0, word_shift * sizeof(StorageType));
            }

            if (bit_shift > 0)
            {
                const SizeType inv_shift = kBitsPerWord - bit_shift;
                for (SizeType i = num_words - 1; i > word_shift; --i)
                {
                    data_[i] <<= bit_shift;
                    data_[i] |= (data_[i - 1] >> inv_shift);
                }
                data_[word_shift] <<= bit_shift;
            }

            ClearUnusedBits();
            return *this;
        }

        /// @brief Shifts all bits right by count positions (efficient word-level shift).
        ///
        /// @param count The number of positions to shift.
        /// @return A reference to this bitset.
        Bitset& operator>>=(SizeType count) noexcept
        {
            if (count == 0 || empty())
            {
                return *this;
            }
            if (count >= num_bits_)
            {
                ResetAll();
                return *this;
            }

            const SizeType word_shift = count / kBitsPerWord;
            const SizeType bit_shift = count % kBitsPerWord;
            const SizeType num_words = data_.size();

            if (word_shift > 0)
            {
                const SizeType copy_end = num_words - word_shift;
                std::memmove(data_.data(), data_.data() + word_shift, copy_end * sizeof(StorageType));
                std::memset(data_.data() + copy_end, 0, word_shift * sizeof(StorageType));
            }

            if (bit_shift > 0)
            {
                const SizeType inv_shift = kBitsPerWord - bit_shift;
                const SizeType shift_end = num_words - 1 - word_shift;
                for (SizeType i = 0; i < shift_end; ++i)
                {
                    // ReSharper disable once CppDFANullDereference
                    data_[i] >>= bit_shift;
                    data_[i] |= (data_[i + 1] << inv_shift);
                }
                data_[shift_end] >>= bit_shift;
            }

            return *this;
        }

        /// @brief Creates a new bitset shifted left by count positions.
        ///
        /// @param count The number of positions to shift.
        /// @return A new shifted bitset.
        BASALT_FORCE_INLINE Bitset operator<<(SizeType count) const
        {
            Bitset r = *this;
            r <<= count;
            return r;
        }

        /// @brief Creates a new bitset shifted right by count positions.
        ///
        /// @param count The number of positions to shift.
        /// @return A new shifted bitset.
        BASALT_FORCE_INLINE Bitset operator>>(SizeType count) const
        {
            Bitset r = *this;
            r >>= count;
            return r;
        }

        /// @brief Checks if two bitsets are equal.
        ///
        /// @param rhs The bitset to compare with.
        /// @return True if both bitsets have the same size and same bits set.
        BASALT_FORCE_INLINE bool operator==(const Bitset& rhs) const noexcept
        {
            if (num_bits_ != rhs.num_bits_)
            {
                return false;
            }
            return data_ == rhs.data_;
        }

        /// @brief Checks if two bitsets are not equal.
        ///
        /// @param rhs The bitset to compare with.
        /// @return True if bitsets differ in size or bits.
        BASALT_FORCE_INLINE bool operator!=(const Bitset& rhs) const noexcept
        {
            return !(*this == rhs);
        }

        /// @brief Returns a string representation of the bitset.
        ///
        /// The string consists of '0' and '1' characters, with the highest index
        /// at the beginning (left) of the string.
        ///
        /// @return A string containing the binary representation.
        [[nodiscard]] std::string ToString() const
        {
            std::string s;
            s.reserve(num_bits_);
            for (SizeType i = 0; i < num_bits_; ++i)
            {
                s.push_back(Test(num_bits_ - 1 - i) ? '1' : '0');
            }
            return s;
        }

        /// @brief Returns a constant reference to the underlying data vector.
        ///
        /// @return The vector containing the bitset words.
        [[nodiscard]] BASALT_FORCE_INLINE const std::vector<StorageType, Alloc>& GetData() const noexcept
        {
            return data_;
        }

        /// @brief Writes the bitset to an output stream.
        ///
        /// @param os The output stream.
        /// @param b The bitset to write.
        /// @return Reference to the output stream.
        friend std::ostream& operator<<(std::ostream& os, const Bitset& b)
        {
            for (SizeType i = b.num_bits_; i > 0; --i)
            {
                os.put(b.Test(i - 1) ? '1' : '0');
            }
            return os;
        }

        template <typename Sink>
            requires AbslStringifySink<Sink>
        friend BASALT_FORCE_INLINE void AbslStringify(Sink& sink, const Bitset& interval) noexcept
        {
            for (SizeType i = interval.num_bits_; i > 0; --i)
            {
                sink.Append(interval.Test(i - 1) ? "1" : "0");
            }
        }

    private:
        static constexpr SizeType kBitsPerWord = sizeof(StorageType) * 8;

        BASALT_FORCE_INLINE void ClearUnusedBits()
        {
            if (num_bits_ == 0)
            {
                return;
            }
            const SizeType extra_bits = num_bits_ % kBitsPerWord;
            if (extra_bits != 0)
            {
                data_.back() &= (StorageType{1} << extra_bits) - 1;
            }
        }

        std::vector<StorageType, Alloc> data_;
        SizeType num_bits_ = 0;
    };
} // namespace bslt

#endif // BASALT_CONTAINER_BITSET_H_
