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
    // Helper to get intervals from a set, now templated on storage
    template <typename T, IntervalSetStorage Storage>
    std::vector<ClosedOpenInterval<T>> GetIntervals(const IntervalSet<T, Storage>& set)
    {
        return std::vector<ClosedOpenInterval<T>>(set.begin(), set.end());
    }

    template <typename T>
    class IntervalSetVectorTypedTest : public ::testing::Test
    {
    };

    using TestTypes = ::testing::Types<int, size_t, int64_t>;
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
        set.Insert(Interval(0, 10));
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

        set.Insert(Interval(10, 20));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(10, 20)));

        // Insert after
        set.Insert(Interval(30, 40));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(10, 20), Interval(30, 40)));

        // Insert before
        set.Insert(Interval(0, 5));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(0, 5), Interval(10, 20), Interval(30, 40)));

        // Insert between
        set.Insert(Interval(25, 28));
        EXPECT_THAT(GetIntervals(set),
                    ::testing::ElementsAre(Interval(0, 5), Interval(10, 20), Interval(25, 28), Interval(30, 40)));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, InsertEmpty)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(5, 5)); // Empty interval
        EXPECT_THAT(GetIntervals(set), ::testing::IsEmpty());
    }

    TYPED_TEST(IntervalSetVectorTypedTest, InsertAdjacent)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;

        set.Insert(Interval(10, 20));

        // Adjacent after
        set.Insert(Interval(20, 30));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(10, 30)));

        // Adjacent before
        set.Insert(Interval(0, 10));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(0, 30)));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, InsertOverlapping)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;

        set.Insert(Interval(10, 20));

        // Overlap end
        set.Insert(Interval(15, 25));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(10, 25)));

        // Overlap start
        set.Insert(Interval(5, 15));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(5, 25)));

        // Superset
        set.Insert(Interval(0, 100));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(0, 100)));

        // Subset
        set.Insert(Interval(10, 20));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(0, 100)));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, InsertComplexMerge)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;

        set.Insert(Interval(0, 10));
        set.Insert(Interval(20, 30));
        set.Insert(Interval(40, 50));
        set.Insert(Interval(60, 70));
        ASSERT_THAT(GetIntervals(set),
                    ::testing::ElementsAre(Interval(0, 10), Interval(20, 30), Interval(40, 50), Interval(60, 70)));

        // Insert interval that spans two existing intervals and touches a third
        set.Insert(Interval(5, 40));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(0, 50), Interval(60, 70)));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, EraseDisjoint)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(10, 20));
        set.Insert(Interval(30, 40));

        // Erase before
        set.Erase(Interval(0, 5));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(10, 20), Interval(30, 40)));

        // Erase after
        set.Erase(Interval(50, 60));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(10, 20), Interval(30, 40)));

        // Erase between
        set.Erase(Interval(25, 28));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(10, 20), Interval(30, 40)));

        // Erase from empty
        IntervalSet<T, IntervalSetStorage::kVector> empty_set;
        empty_set.Erase(Interval(0, 10));
        EXPECT_THAT(GetIntervals(empty_set), ::testing::IsEmpty());
    }

    TYPED_TEST(IntervalSetVectorTypedTest, EraseAdjacent)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(10, 20));

        // Erase adjacent (touching)
        set.Erase(Interval(5, 10)); // Touches start
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(10, 20)));

        set.Erase(Interval(20, 25)); // Touches end
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(10, 20)));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, ErasePartial)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(10, 20));

        // Erase start
        set.Erase(Interval(5, 15));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(15, 20)));

        // Reset
        set.clear();
        set.Insert(Interval(10, 20));

        // Erase end
        set.Erase(Interval(15, 25));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(10, 15)));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, EraseCausesSplit)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(0, 100));

        set.Erase(Interval(40, 60));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(0, 40), Interval(60, 100)));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, EraseFullAndSuperset)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(10, 20));

        // Erase exact match
        set.Erase(Interval(10, 20));
        EXPECT_THAT(GetIntervals(set), ::testing::IsEmpty());

        // Erase superset
        set.Insert(Interval(10, 20));
        set.Erase(Interval(0, 100));
        EXPECT_THAT(GetIntervals(set), ::testing::IsEmpty());
    }

    TYPED_TEST(IntervalSetVectorTypedTest, EraseComplex)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(0, 10));
        set.Insert(Interval(20, 30));
        set.Insert(Interval(40, 50));
        set.Insert(Interval(60, 70));
        ASSERT_THAT(GetIntervals(set),
                    ::testing::ElementsAre(Interval(0, 10), Interval(20, 30), Interval(40, 50), Interval(60, 70)));

        // Erase spanning multiple, splitting first and last
        set.Erase(Interval(5, 65));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(0, 5), Interval(65, 70)));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, ContainsValue)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(10, 20));
        set.Insert(Interval(30, 40));

        // In
        EXPECT_TRUE(set.Contains(T{10})); // Start
        EXPECT_TRUE(set.Contains(T{15})); // Middle
        EXPECT_TRUE(set.Contains(T{19})); // Just before end
        EXPECT_TRUE(set.Contains(T{30})); // Start of second

        // Out
        EXPECT_FALSE(set.Contains(T{9})); // Just before start
        EXPECT_FALSE(set.Contains(T{20})); // End
        EXPECT_FALSE(set.Contains(T{25})); // Between
        EXPECT_FALSE(set.Contains(T{40})); // End of second
        EXPECT_FALSE(set.Contains(T{100})); // Far away
    }

    TYPED_TEST(IntervalSetVectorTypedTest, ContainsInterval)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(0, 100));

        // True
        EXPECT_TRUE(set.ContainsInterval(Interval(10, 20))); // Subset
        EXPECT_TRUE(set.ContainsInterval(Interval(0, 100))); // Exact match
        EXPECT_TRUE(set.ContainsInterval(Interval(0, 50))); // Touches start
        EXPECT_TRUE(set.ContainsInterval(Interval(50, 100))); // Touches end
        EXPECT_TRUE(set.ContainsInterval(Interval(50, 50))); // Empty

        // False
        EXPECT_FALSE(set.ContainsInterval(Interval(0, 101))); // Overlap end
        EXPECT_FALSE(set.ContainsInterval(Interval(-1, 50))); // Overlap start
        EXPECT_FALSE(set.ContainsInterval(Interval(-5, 105))); // Superset
        EXPECT_FALSE(set.ContainsInterval(Interval(100, 110))); // Adjacent
        EXPECT_FALSE(set.ContainsInterval(Interval(110, 120))); // Disjoint
    }

    TYPED_TEST(IntervalSetVectorTypedTest, ContainsIntervalSpanningMultiple)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(0, 10));
        set.Insert(Interval(10, 20)); // Set is [0, 20)
        EXPECT_TRUE(set.ContainsInterval(Interval(5, 15)));
    }


    TYPED_TEST(IntervalSetVectorTypedTest, Intersects)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(10, 20));
        set.Insert(Interval(30, 40));

        // True
        EXPECT_TRUE(set.Intersects(Interval(5, 15))); // Overlap start
        EXPECT_TRUE(set.Intersects(Interval(15, 25))); // Overlap end
        EXPECT_TRUE(set.Intersects(Interval(12, 18))); // Subset
        EXPECT_TRUE(set.Intersects(Interval(5, 25))); // Superset
        EXPECT_TRUE(set.Intersects(Interval(25, 35))); // Spans gap, intersects second
        EXPECT_TRUE(set.Intersects(Interval(0, 100))); // Spans all

        // False
        EXPECT_FALSE(set.Intersects(Interval(0, 5))); // Disjoint
        EXPECT_FALSE(set.Intersects(Interval(20, 30))); // Disjoint (gap)
        EXPECT_FALSE(set.Intersects(Interval(50, 60))); // Disjoint

        // Adjacent is NOT intersecting
        EXPECT_FALSE(set.Intersects(Interval(5, 10)));
        EXPECT_FALSE(set.Intersects(Interval(20, 25)));
    }

    // =========================================================================
    // Public API Conversion Tests (using int, kVector)
    // =========================================================================

    class IntervalSetApiVectorTest : public ::testing::Test
    {
    };

    TEST_F(IntervalSetApiVectorTest, InsertAllTypes)
    {
        IntervalSet<int, IntervalSetStorage::kVector> set;

        // [0, 10] -> [0, 11)
        set.Insert(ClosedInterval(0, 10));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(ClosedOpenInterval(0, 11)));

        // (10, 20] -> [11, 21). Merges with [0, 11) -> [0, 21)
        set.Insert(OpenClosedInterval(10, 20));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(ClosedOpenInterval(0, 21)));

        // (30, 40) -> [31, 40)
        set.Insert(OpenInterval(30, 40));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(ClosedOpenInterval(0, 21), ClosedOpenInterval(31, 40)));

        // (20, 31) -> [21, 31). Merges both -> [0, 40)
        set.Insert(OpenInterval(20, 31));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(ClosedOpenInterval(0, 40)));
    }

    TEST_F(IntervalSetApiVectorTest, EraseAllTypes)
    {
        IntervalSet<int, IntervalSetStorage::kVector> set;
        set.Insert(ClosedOpenInterval(0, 100));

        // Erase [10, 20] -> Erase [10, 21)
        set.Erase(ClosedInterval(10, 20));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(ClosedOpenInterval(0, 10), ClosedOpenInterval(21, 100)));

        // Erase (30, 40) -> Erase [31, 40)
        set.Erase(OpenInterval(30, 40));
        EXPECT_THAT(GetIntervals(set),
                    ::testing::ElementsAre(ClosedOpenInterval(0, 10), ClosedOpenInterval(21, 31),
                        ClosedOpenInterval(40, 100)));

        // Erase (50, 60] -> Erase [51, 61)
        set.Erase(OpenClosedInterval(50, 60));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(ClosedOpenInterval(0, 10), ClosedOpenInterval(21, 31),
                        ClosedOpenInterval(40, 51), ClosedOpenInterval(61, 100)));
    }

    TEST_F(IntervalSetApiVectorTest, ContainsIntervalAllTypes)
    {
        IntervalSet<int, IntervalSetStorage::kVector> set;
        set.Insert(ClosedOpenInterval(10, 30));

        // Set is [10, 30)
        EXPECT_TRUE(set.ContainsInterval(ClosedInterval(10, 29)));
        EXPECT_FALSE(set.ContainsInterval(ClosedInterval(10, 30)));
        EXPECT_TRUE(set.ContainsInterval(OpenInterval(10, 29)));
        EXPECT_TRUE(set.ContainsInterval(OpenInterval(9, 29)));
        EXPECT_FALSE(set.ContainsInterval(OpenInterval(8, 29)));
    }

    TEST_F(IntervalSetApiVectorTest, IntersectsAllTypes)
    {
        IntervalSet<int, IntervalSetStorage::kVector> set;
        set.Insert(ClosedOpenInterval(10, 20));

        // Set is [10, 20)
        EXPECT_TRUE(set.Intersects(ClosedInterval(5, 10)));
        EXPECT_FALSE(set.Intersects(ClosedInterval(5, 9)));
        EXPECT_FALSE(set.Intersects(OpenInterval(19, 25)));
        EXPECT_TRUE(set.Intersects(OpenInterval(18, 25)));
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
        EXPECT_THAT(GetIntervals(a), ::testing::ElementsAre(
                        Interval(0, 15), Interval(20, 40), Interval(50, 60), Interval(100, 110)));
        const auto C = a + IntervalSet<int, IntervalSetStorage::kVector>();
        EXPECT_THAT(GetIntervals(C), ::testing::ElementsAre(
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
        auto C = a;
        C.Erase(Interval(25, 32));
        EXPECT_THAT(GetIntervals(C), ::testing::ElementsAre(
                        Interval(5, 10), Interval(20, 25), Interval(35, 90)));
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

    TEST_F(IntervalSetOperationsVectorTest, IntersectionMerge)
    {
        a.Insert(Interval(0, 10));
        a.Insert(Interval(10, 20)); // A = [0, 20)
        b.Insert(Interval(5, 15));
        const auto C = a & b;
        EXPECT_THAT(GetIntervals(C), ::testing::ElementsAre(Interval(5, 15)));
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
        a.clear();
        a.Insert(Interval(0, 10));
        a.Clip(Interval(100, 110));
        EXPECT_THAT(GetIntervals(a), ::testing::IsEmpty());
        a.Insert(Interval(0, 10));
        a.Clip(Interval(5, 5));
        EXPECT_THAT(GetIntervals(a), ::testing::IsEmpty());
    }

    TEST_F(IntervalSetOperationsVectorTest, UnionAdjacentMerge)
    {
        // A = [0, 10), [20, 30)
        a.Insert(Interval(0, 10));
        a.Insert(Interval(20, 30));
        // B = [10, 20), [40, 50)
        b.Insert(Interval(10, 20));
        b.Insert(Interval(40, 50));

        // Expected result: [0, 30), [40, 50)
        a.Insert(b);
        EXPECT_THAT(GetIntervals(a), ::testing::ElementsAre(
                        Interval(0, 30), Interval(40, 50)));

        // Test with operator+
        auto c = IntervalSet<int, IntervalSetStorage::kVector>();
        c.Insert(Interval(100, 110));
        c.Insert(Interval(120, 130));
        auto d = IntervalSet<int, IntervalSetStorage::kVector>();
        d.Insert(Interval(110, 120));
        const auto e = c + d;
        EXPECT_THAT(GetIntervals(e), ::testing::ElementsAre(Interval(100, 130)));
    }

    TEST_F(IntervalSetOperationsVectorTest, DifferenceSplitAndRemove)
    {
        // A = [0, 50), [60, 70), [80, 90)
        a.Insert(Interval(0, 50));
        a.Insert(Interval(60, 70));
        a.Insert(Interval(80, 90));

        // B = [20, 30) (Splits [0, 50) into [0, 20) and [30, 50))
        // B = [60, 70) (Removes [60, 70) entirely)
        // B = [85, 95) (Trims [80, 90) to [80, 85))
        b.Insert(Interval(20, 30));
        b.Insert(Interval(60, 70));
        b.Insert(Interval(85, 95));

        a.Erase(b);
        EXPECT_THAT(GetIntervals(a), ::testing::ElementsAre(
                        Interval(0, 20), Interval(30, 50), Interval(80, 85)));

        // Test subtraction operator
        IntervalSet<int> x;
        x.Insert(Interval(100, 200));
        IntervalSet<int> y;
        y.Insert(Interval(100, 150));
        const auto z = x - y;
        EXPECT_THAT(GetIntervals(z), ::testing::ElementsAre(Interval(150, 200)));
    }

    TEST_F(IntervalSetOperationsVectorTest, IntersectionFullOverlap)
    {
        // A = [0, 10), [20, 30)
        a.Insert(Interval(0, 10));
        a.Insert(Interval(20, 30));

        // B = A
        b = a;

        a.Intersection(b);
        EXPECT_THAT(GetIntervals(a), ::testing::ElementsAre(
                        Interval(0, 10), Interval(20, 30)));

        // Test operator&
        IntervalSet<int> x;
        x.Insert(Interval(100, 150));
        IntervalSet<int> y;
        y.Insert(Interval(100, 150));
        const auto Z = x & y;
        EXPECT_THAT(GetIntervals(Z), ::testing::ElementsAre(Interval(100, 150)));
    }

    TEST_F(IntervalSetOperationsVectorTest, ClipClosedIntervalConversion)
    {
        IntervalSet<int, IntervalSetStorage::kVector> set;
        set.Insert(Interval(0, 10));
        set.Insert(Interval(20, 30));
        set.Insert(Interval(40, 50));

        // Clip to [5, 45] -> canonical [5, 46)
        set.Clip(ClosedInterval(5, 45));

        // Expected: [5, 10), [20, 30), [40, 46)
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(
                        Interval(5, 10), Interval(20, 30), Interval(40, 46)));

        // Clear and test open interval
        set.clear();
        set.Insert(Interval(100, 200));

        // Clip to (110, 190) -> canonical [111, 190)
        set.Clip(OpenInterval(110, 190));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(111, 190)));

        // Clear and test open-closed interval
        set.clear();
        set.Insert(Interval(100, 200));

        // Clip to (120, 180] -> canonical [121, 181)
        set.Clip(OpenClosedInterval(120, 180));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(121, 181)));
    }
}
