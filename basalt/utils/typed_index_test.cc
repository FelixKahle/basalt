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

#include "basalt/utils/typed_index.h"
#include <gtest/gtest.h>
#include <type_traits>
#include <sstream>
#include <unordered_set>

namespace bslt::test
{
    struct EntityTag
    {
    };

    struct MeshTag
    {
    };

    using EntityId = TypedIndex<EntityTag>;
    using MeshId = TypedIndex<MeshTag>;

    TEST(TypedIndexTest, DefaultConstructor)
    {
        constexpr EntityId id;
        EXPECT_EQ(id.Index(), 0);
        EXPECT_EQ(static_cast<std::size_t>(id), 0);
    }

    TEST(TypedIndexTest, ExplicitConstructor)
    {
        constexpr EntityId id(42);
        EXPECT_EQ(id.Index(), 42);
        EXPECT_EQ(static_cast<std::size_t>(id), 42);
    }

    TEST(TypedIndexTest, EqualityOperators)
    {
        constexpr EntityId id1(10);
        constexpr EntityId id2(10);
        constexpr EntityId id3(20);

        EXPECT_TRUE(id1 == id2);
        EXPECT_FALSE(id1 == id3);
        EXPECT_TRUE(id1 != id3);
        EXPECT_FALSE(id1 != id2);
    }

    TEST(TypedIndexTest, RelationalOperators)
    {
        constexpr EntityId small(10);
        constexpr EntityId large(20);

        EXPECT_TRUE(small < large);
        EXPECT_TRUE(small <= large);
        EXPECT_TRUE(small <= small);

        EXPECT_TRUE(large > small);
        EXPECT_TRUE(large >= small);
        EXPECT_TRUE(large >= large);

        EXPECT_FALSE(small > large);
        EXPECT_FALSE(large < small);
    }

    TEST(TypedIndexTest, IncrementOperators)
    {
        EntityId id(10);

        // Pre-increment
        EntityId& ref = ++id;
        EXPECT_EQ(id.Index(), 11);
        EXPECT_EQ(ref.Index(), 11);
        EXPECT_EQ(&ref, &id); // Should return reference to self

        // Post-increment
        const EntityId old = id++;
        EXPECT_EQ(old.Index(), 11);
        EXPECT_EQ(id.Index(), 12);
    }

    TEST(TypedIndexTest, DecrementOperators)
    {
        EntityId id(10);

        // Pre-decrement
        EntityId& ref = --id;
        EXPECT_EQ(id.Index(), 9);
        EXPECT_EQ(ref.Index(), 9);
        EXPECT_EQ(&ref, &id);

        // Post-decrement
        const EntityId old = id--;
        EXPECT_EQ(old.Index(), 9);
        EXPECT_EQ(id.Index(), 8);
    }

    TEST(TypedIndexTest, StreamOperator)
    {
        constexpr EntityId id(12345);
        std::stringstream ss;
        ss << id;
        EXPECT_EQ(ss.str(), "12345");
    }

    TEST(TypedIndexTest, StdHashSupport)
    {
        // Verify it can be used in standard containers requiring hash
        std::unordered_set<EntityId> set;
        set.insert(EntityId(1));
        set.insert(EntityId(2));
        set.insert(EntityId(1)); // Duplicate

        EXPECT_EQ(set.size(), 2);
        EXPECT_TRUE(set.contains(EntityId(1)));
        EXPECT_FALSE(set.contains(EntityId(3)));

        // Verify hash consistency
        constexpr std::hash<EntityId> hasher;
        EXPECT_EQ(hasher(EntityId(42)), std::hash<std::size_t>{}(42));
    }

    TEST(TypedIndexTest, TypeSafety)
    {
        // These checks are primarily compile-time, but we verify basic type traits here.
        // EntityId and MeshId should be distinct types.
        EXPECT_FALSE((std::is_same_v<EntityId, MeshId>));

        // Ensure they are not implicitly convertible (conceptually)
        // Note: standard doesn't easily test "explicit" ctor failure at runtime,
        // but is_convertible checks implicit conversion.
        EXPECT_FALSE((std::is_convertible_v<std::size_t, EntityId>));

        // However, it should be explicitly constructible
        EXPECT_TRUE((std::is_constructible_v<EntityId, std::size_t>));
    }
} // namespace bslt::test
