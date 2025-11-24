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

#ifndef BASALT_UTILS_TYPED_INDEX_H_
#define BASALT_UTILS_TYPED_INDEX_H_

#include <cstddef>
#include <functional>
#include "basalt/base/config.h"
#include "basalt/type_traits/type_traits.h"

namespace bslt
{
    /// @brief A type-safe index class template.
    ///
    /// This is a type safe wrapper around a std::size_t index value.
    /// It provides type safe index with a zero-overhead abstraction.
    /// Access the underlying index value via the Index() method
    /// or the explicit conversion operator to std::size_t.
    template <typename TagType>
    class TypedIndex
    {
    public:
        /// @brief The underlying index type.
        using IndexType = std::size_t;
        /// @brief The tag type used for type safety.
        using Tag = TagType;

        /// @brief Default constructor.
        ///
        /// Creates an invalid TypedIndex with index value 0.
        constexpr BASALT_FORCE_INLINE TypedIndex() noexcept
            : index_(static_cast<std::size_t>(0))
        {
        }

        /// @brief Constructs a TypedIndex from a std::size_t index value.
        ///
        /// @param index The index value.
        constexpr BASALT_FORCE_INLINE explicit TypedIndex(const std::size_t index) noexcept
            : index_(index)
        {
        }

        /// @brief Retrieves the underlying index value.
        ///
        /// @return The index value as std::size_t.
        [[nodiscard]] constexpr BASALT_FORCE_INLINE IndexType Index() const noexcept
        {
            return index_;
        }

        /// @brief Explicit conversion operator to std::size_t.
        ///
        /// @return The index value as std::size_t.
        explicit constexpr operator IndexType() const noexcept
        {
            return index_;
        }

        friend constexpr BASALT_FORCE_INLINE bool operator==(const TypedIndex lhs, const TypedIndex rhs) noexcept
        {
            return lhs.index_ == rhs.index_;
        }

        friend constexpr BASALT_FORCE_INLINE bool operator!=(const TypedIndex lhs, const TypedIndex rhs) noexcept
        {
            return lhs.index_ != rhs.index_;
        }

        friend constexpr BASALT_FORCE_INLINE bool operator<(const TypedIndex lhs, const TypedIndex rhs) noexcept
        {
            return lhs.index_ < rhs.index_;
        }

        friend constexpr BASALT_FORCE_INLINE bool operator>(const TypedIndex lhs, const TypedIndex rhs) noexcept
        {
            return lhs.index_ > rhs.index_;
        }

        friend constexpr BASALT_FORCE_INLINE bool operator<=(const TypedIndex lhs, const TypedIndex rhs) noexcept
        {
            return lhs.index_ <= rhs.index_;
        }

        friend constexpr BASALT_FORCE_INLINE bool operator>=(const TypedIndex lhs, const TypedIndex rhs) noexcept
        {
            return lhs.index_ >= rhs.index_;
        }

        constexpr BASALT_FORCE_INLINE TypedIndex& operator++() noexcept
        {
            ++index_;
            return *this;
        }

        constexpr BASALT_FORCE_INLINE TypedIndex operator++(int) noexcept
        {
            TypedIndex temp = *this;
            ++index_;
            return temp;
        }

        constexpr BASALT_FORCE_INLINE TypedIndex& operator--() noexcept
        {
            --index_;
            return *this;
        }

        constexpr BASALT_FORCE_INLINE TypedIndex operator--(int) noexcept
        {
            TypedIndex temp = *this;
            --index_;
            return temp;
        }

        friend std::ostream& operator<<(std::ostream& os, const TypedIndex idx)
        {
            return os << idx.Index();
        }

        template <typename H>
            requires AbslHasher<H>
        friend constexpr BASALT_FORCE_INLINE H AbslHashValue(H h, const TypedIndex index) noexcept
        {
            return H::combine(std::move(h), index.index_);
        }

        template <typename Sink>
            requires AbslStringifySink<Sink>
        friend BASALT_FORCE_INLINE void AbslStringify(Sink& sink, const TypedIndex index) noexcept
        {
            absl::Format(&sink, "%v", index.Index());
        }

    private:
        IndexType index_;
    };
} // namespace bslt

template <typename TagType>
struct std::hash<bslt::TypedIndex<TagType>>
{
    std::size_t operator()(const bslt::TypedIndex<TagType>& key) const noexcept
    {
        return std::hash<std::size_t>{}(key.Index());
    }
};

#endif // BASALT_UTILS_TYPED_INDEX_H_
