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

#include "basalt/container/interval_set.h"
#include "gtest/gtest.h"
#include "gmock/gmock.h"
#include <vector>

namespace bslt::test
{
    // Helper to get intervals from a set
    template <typename T, IntervalSetStorage Storage>
    std::vector<ClosedOpenInterval<T>> GetIntervals(const IntervalSet<T, Storage>& set)
    {
        return std::vector<ClosedOpenInterval<T>>(set.begin(), set.end());
    }

    // =========================================================================
    // Typed Tests (Vector Backend)
    // =========================================================================

    template <typename T>
    class IntervalSetVectorTypedTest : public ::testing::Test
    {
    };

    // Added float and double to the test types
    using TestTypes = ::testing::Types<int, size_t, int64_t, float, double>;
    TYPED_TEST_SUITE(IntervalSetVectorTypedTest, TestTypes);

    TYPED_TEST(IntervalSetVectorTypedTest, Construction)
    {
        using T = TypeParam;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        EXPECT_TRUE(set.IsEmpty());
        EXPECT_EQ(set.size(), 0);
        EXPECT_EQ(set.begin(), set.end());
    }

    TYPED_TEST(IntervalSetVectorTypedTest, Clear)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(static_cast<T>(0), static_cast<T>(10)));
        ASSERT_FALSE(set.IsEmpty());

        set.clear();
        EXPECT_TRUE(set.IsEmpty());
        EXPECT_EQ(set.size(), 0);
    }

    TYPED_TEST(IntervalSetVectorTypedTest, InsertDisjoint)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;

        set.Insert(Interval(static_cast<T>(10), static_cast<T>(20)));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(static_cast<T>(10), static_cast<T>(20))));

        // Insert after
        set.Insert(Interval(static_cast<T>(30), static_cast<T>(40)));
        EXPECT_THAT(GetIntervals(set),
                    ::testing::ElementsAre(Interval(static_cast<T>(10), static_cast<T>(20)), Interval(static_cast<T>(30)
                        , static_cast<T>(40))));

        // Insert before
        set.Insert(Interval(static_cast<T>(0), static_cast<T>(5)));
        EXPECT_THAT(GetIntervals(set),
                    ::testing::ElementsAre(Interval(static_cast<T>(0), static_cast<T>(5)), Interval(static_cast<T>(10),
                        static_cast<T>(20)), Interval(static_cast<T>(30), static_cast<T>(40))));

        // Insert between
        set.Insert(Interval(static_cast<T>(25), static_cast<T>(28)));
        EXPECT_THAT(GetIntervals(set),
                    ::testing::ElementsAre(Interval(static_cast<T>(0), static_cast<T>(5)), Interval(static_cast<T>(10),
                        static_cast<T>(20)), Interval(static_cast<T>(25), static_cast<T>(28)), Interval(static_cast<T>(
                        30), static_cast<T>(40))));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, InsertEmpty)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(static_cast<T>(5), static_cast<T>(5))); // Empty interval
        EXPECT_THAT(GetIntervals(set), ::testing::IsEmpty());
    }

    TYPED_TEST(IntervalSetVectorTypedTest, InsertAdjacent)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;

        set.Insert(Interval(static_cast<T>(10), static_cast<T>(20)));

        // Adjacent after [10, 20) + [20, 30) -> [10, 30)
        set.Insert(Interval(static_cast<T>(20), static_cast<T>(30)));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(static_cast<T>(10), static_cast<T>(30))));

        // Adjacent before [0, 10) + [10, 30) -> [0, 30)
        set.Insert(Interval(static_cast<T>(0), static_cast<T>(10)));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(static_cast<T>(0), static_cast<T>(30))));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, InsertOverlapping)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;

        set.Insert(Interval(static_cast<T>(10), static_cast<T>(20)));

        // Overlap end
        set.Insert(Interval(static_cast<T>(15), static_cast<T>(25)));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(static_cast<T>(10), static_cast<T>(25))));

        // Overlap start
        set.Insert(Interval(static_cast<T>(5), static_cast<T>(15)));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(static_cast<T>(5), static_cast<T>(25))));

        // Superset
        set.Insert(Interval(static_cast<T>(0), static_cast<T>(100)));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(static_cast<T>(0), static_cast<T>(100))));

        // Subset
        set.Insert(Interval(static_cast<T>(10), static_cast<T>(20)));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(static_cast<T>(0), static_cast<T>(100))));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, InsertComplexMerge)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;

        set.Insert(Interval(static_cast<T>(0), static_cast<T>(10)));
        set.Insert(Interval(static_cast<T>(20), static_cast<T>(30)));
        set.Insert(Interval(static_cast<T>(40), static_cast<T>(50)));
        set.Insert(Interval(static_cast<T>(60), static_cast<T>(70)));
        ASSERT_THAT(GetIntervals(set),
                    ::testing::ElementsAre(Interval(static_cast<T>(0), static_cast<T>(10)), Interval(static_cast<T>(20),
                        static_cast<T>(30)), Interval(static_cast<T>(40), static_cast<T>(50)), Interval(static_cast<T>(
                        60), static_cast<T>(70))));

        // Insert interval that spans two existing intervals and touches a third
        // [5, 40) spans across [20,30) and meets [40,50) at 40.
        // Result should be [0, 50) and [60, 70)
        set.Insert(Interval(static_cast<T>(5), static_cast<T>(40)));
        EXPECT_THAT(GetIntervals(set),
                    ::testing::ElementsAre(Interval(static_cast<T>(0), static_cast<T>(50)), Interval(static_cast<T>(60),
                        static_cast<T>(70))));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, EraseDisjoint)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(static_cast<T>(10), static_cast<T>(20)));
        set.Insert(Interval(static_cast<T>(30), static_cast<T>(40)));

        // Erase before
        set.Erase(Interval(static_cast<T>(0), static_cast<T>(5)));
        EXPECT_THAT(GetIntervals(set),
                    ::testing::ElementsAre(Interval(static_cast<T>(10), static_cast<T>(20)), Interval(static_cast<T>(30)
                        , static_cast<T>(40))));

        // Erase after
        set.Erase(Interval(static_cast<T>(50), static_cast<T>(60)));
        EXPECT_THAT(GetIntervals(set),
                    ::testing::ElementsAre(Interval(static_cast<T>(10), static_cast<T>(20)), Interval(static_cast<T>(30)
                        , static_cast<T>(40))));

        // Erase between
        set.Erase(Interval(static_cast<T>(25), static_cast<T>(28)));
        EXPECT_THAT(GetIntervals(set),
                    ::testing::ElementsAre(Interval(static_cast<T>(10), static_cast<T>(20)), Interval(static_cast<T>(30)
                        , static_cast<T>(40))));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, EraseAdjacent)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(static_cast<T>(10), static_cast<T>(20)));

        // Erase adjacent (touching)
        set.Erase(Interval(static_cast<T>(5), static_cast<T>(10))); // Touches start
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(static_cast<T>(10), static_cast<T>(20))));

        set.Erase(Interval(static_cast<T>(20), static_cast<T>(25))); // Touches end
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(static_cast<T>(10), static_cast<T>(20))));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, ErasePartial)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(static_cast<T>(10), static_cast<T>(20)));

        // Erase start
        set.Erase(Interval(static_cast<T>(5), static_cast<T>(15)));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(static_cast<T>(15), static_cast<T>(20))));

        // Reset
        set.clear();
        set.Insert(Interval(static_cast<T>(10), static_cast<T>(20)));

        // Erase end
        set.Erase(Interval(static_cast<T>(15), static_cast<T>(25)));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(static_cast<T>(10), static_cast<T>(15))));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, EraseCausesSplit)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(static_cast<T>(0), static_cast<T>(100)));

        set.Erase(Interval(static_cast<T>(40), static_cast<T>(60)));
        EXPECT_THAT(GetIntervals(set),
                    ::testing::ElementsAre(Interval(static_cast<T>(0), static_cast<T>(40)), Interval(static_cast<T>(60),
                        static_cast<T>(100))));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, EraseFullAndSuperset)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(static_cast<T>(10), static_cast<T>(20)));

        // Erase exact match
        set.Erase(Interval(static_cast<T>(10), static_cast<T>(20)));
        EXPECT_THAT(GetIntervals(set), ::testing::IsEmpty());

        // Erase superset
        set.Insert(Interval(static_cast<T>(10), static_cast<T>(20)));
        set.Erase(Interval(static_cast<T>(0), static_cast<T>(100)));
        EXPECT_THAT(GetIntervals(set), ::testing::IsEmpty());
    }

    TYPED_TEST(IntervalSetVectorTypedTest, ContainsValue)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(static_cast<T>(10), static_cast<T>(20)));
        set.Insert(Interval(static_cast<T>(30), static_cast<T>(40)));

        // In
        EXPECT_TRUE(set.Contains(T{10})); // Start
        EXPECT_TRUE(set.Contains(T{15})); // Middle
        EXPECT_TRUE(set.Contains(T{30})); // Start of second

        // Out
        EXPECT_FALSE(set.Contains(T{9})); // Just before start
        EXPECT_FALSE(set.Contains(T{20})); // End
        EXPECT_FALSE(set.Contains(T{25})); // Between
        EXPECT_FALSE(set.Contains(T{40})); // End of second
    }

    TYPED_TEST(IntervalSetVectorTypedTest, ContainsInterval)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(static_cast<T>(0), static_cast<T>(100)));

        EXPECT_TRUE(set.ContainsInterval(Interval(static_cast<T>(10), static_cast<T>(20)))); // Subset
        EXPECT_TRUE(set.ContainsInterval(Interval(static_cast<T>(0), static_cast<T>(100)))); // Exact match
        EXPECT_FALSE(set.ContainsInterval(Interval(static_cast<T>(0), static_cast<T>(101)))); // Overlap end
    }

    TYPED_TEST(IntervalSetVectorTypedTest, Intersects)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(static_cast<T>(10), static_cast<T>(20)));

        EXPECT_TRUE(set.Intersects(Interval(static_cast<T>(5), static_cast<T>(15)))); // Overlap start
        EXPECT_TRUE(set.Intersects(Interval(static_cast<T>(15), static_cast<T>(25)))); // Overlap end
        EXPECT_FALSE(set.Intersects(Interval(static_cast<T>(0), static_cast<T>(5)))); // Disjoint
        // Adjacent is NOT intersecting
        EXPECT_FALSE(set.Intersects(Interval(static_cast<T>(5), static_cast<T>(10))));
    }

    // =========================================================================
    // Set-Based Operations Tests (using int, kVector)
    // =========================================================================

    class IntervalSetOperationsVectorTest : public ::testing::Test
    {
    protected:
        IntervalSet<int, IntervalSetStorage::kVector> a;
        IntervalSet<int, IntervalSetStorage::kVector> b;
        using Interval = ClosedOpenInterval<int>;
    };

    TEST_F(IntervalSetOperationsVectorTest, Union)
    {
        a.Insert(Interval(0, 10));
        a.Insert(Interval(20, 30));
        a.Insert(Interval(50, 60));
        b.Insert(Interval(5, 15));
        b.Insert(Interval(30, 40));
        b.Insert(Interval(100, 110));
        a.Insert(b);
        // [0, 10) + [5, 15) -> [0, 15)
        // [20, 30) + [30, 40) -> [20, 40) (Adjacent merge)
        EXPECT_THAT(GetIntervals(a), ::testing::ElementsAre(
                        Interval(0, 15), Interval(20, 40), Interval(50, 60), Interval(100, 110)));
    }

    TEST_F(IntervalSetOperationsVectorTest, Difference)
    {
        a.Insert(Interval(0, 100));
        b.Insert(Interval(-10, 5));
        b.Insert(Interval(10, 20));
        b.Insert(Interval(30, 35));
        b.Insert(Interval(90, 110));
        a.Erase(b);
        EXPECT_THAT(GetIntervals(a), ::testing::ElementsAre(
                        Interval(5, 10), Interval(20, 30), Interval(35, 90)));
    }

    TEST_F(IntervalSetOperationsVectorTest, Intersection)
    {
        a.Insert(Interval(0, 10));
        a.Insert(Interval(20, 30));
        a.Insert(Interval(50, 100));
        b.Insert(Interval(5, 25));
        b.Insert(Interval(55, 65));
        b.Insert(Interval(70, 80));
        b.Insert(Interval(99, 101));
        a.Intersection(b);
        EXPECT_THAT(GetIntervals(a), ::testing::ElementsAre(
                        Interval(5, 10), Interval(20, 25), Interval(55, 65), Interval(70, 80), Interval(99, 100)));
    }

    TEST_F(IntervalSetOperationsVectorTest, Clip)
    {
        a.Insert(Interval(0, 10));
        a.Insert(Interval(20, 30));
        a.Insert(Interval(40, 50));
        a.Clip(Interval(-10, 100));
        EXPECT_THAT(GetIntervals(a), ::testing::ElementsAre(Interval(0, 10), Interval(20, 30), Interval(40, 50)));
        a.Clip(Interval(5, 45));
        EXPECT_THAT(GetIntervals(a), ::testing::ElementsAre(Interval(5, 10), Interval(20, 30), Interval(40, 45)));
    }

    // =========================================================================
    // Typed Tests (BTree Backend)
    // =========================================================================

    template <typename T>
    class IntervalSetBTreeTypedTest : public ::testing::Test
    {
    };

    TYPED_TEST_SUITE(IntervalSetBTreeTypedTest, TestTypes);

    TYPED_TEST(IntervalSetBTreeTypedTest, Construction)
    {
        using T = TypeParam;
        IntervalSet<T, IntervalSetStorage::kBTree> set;
        EXPECT_TRUE(set.IsEmpty());
    }

    TYPED_TEST(IntervalSetBTreeTypedTest, InsertDisjointAndMerge)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kBTree> set;

        set.Insert(Interval(static_cast<T>(10), static_cast<T>(20)));
        set.Insert(Interval(static_cast<T>(20), static_cast<T>(30))); // Adjacent
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(static_cast<T>(10), static_cast<T>(30))));
    }

    TYPED_TEST(IntervalSetBTreeTypedTest, EraseSplitting)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kBTree> set;
        set.Insert(Interval(static_cast<T>(0), static_cast<T>(100)));
        set.Erase(Interval(static_cast<T>(40), static_cast<T>(60)));
        EXPECT_THAT(GetIntervals(set),
                    ::testing::ElementsAre(Interval(static_cast<T>(0), static_cast<T>(40)), Interval(static_cast<T>(60),
                        static_cast<T>(100))));
    }

    TYPED_TEST(IntervalSetBTreeTypedTest, ContainsInterval)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kBTree> set;
        set.Insert(Interval(static_cast<T>(0), static_cast<T>(100)));
        EXPECT_TRUE(set.ContainsInterval(Interval(static_cast<T>(10), static_cast<T>(20))));
        EXPECT_FALSE(set.ContainsInterval(Interval(static_cast<T>(0), static_cast<T>(101))));
    }

    // =========================================================================
    // Set-Based Operations Tests (using int, kBTree)
    // =========================================================================

    class IntervalSetOperationsBTreeTest : public ::testing::Test
    {
    protected:
        IntervalSet<int, IntervalSetStorage::kBTree> a;
        IntervalSet<int, IntervalSetStorage::kBTree> b;
        using Interval = ClosedOpenInterval<int>;
    };

    TEST_F(IntervalSetOperationsBTreeTest, Union)
    {
        a.Insert(Interval(0, 10));
        b.Insert(Interval(5, 15));
        a.Insert(b);
        EXPECT_THAT(GetIntervals(a), ::testing::ElementsAre(Interval(0, 15)));
    }

    TEST_F(IntervalSetOperationsBTreeTest, Difference)
    {
        a.Insert(Interval(0, 100));
        b.Insert(Interval(40, 60));
        a.Erase(b);
        EXPECT_THAT(GetIntervals(a), ::testing::ElementsAre(Interval(0, 40), Interval(60, 100)));
    }
}
