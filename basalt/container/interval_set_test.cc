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
#include <random>

namespace bslt::test
{
    template <typename T, IntervalSetStorage Storage>
    static std::vector<ClosedOpenInterval<T>> GetIntervals(const IntervalSet<T, Storage>& set)
    {
        return std::vector<ClosedOpenInterval<T>>(set.begin(), set.end());
    }

    template <typename T>
    class IntervalSetVectorTypedTest : public ::testing::Test
    {
    };

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

    template <typename T>
    class IntervalSetRangesTest : public ::testing::Test
    {
    };

    TYPED_TEST_SUITE(IntervalSetRangesTest, TestTypes);

    TYPED_TEST(IntervalSetRangesTest, RangeConstructorViews)
    {
        // Test compatibility with std::views pipeline
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;

        std::vector<T> starts = {0, 20, 40};

        // Transform vector of ints into vector of Intervals [x, x+10)
        auto view = starts | std::views::transform([](T x) { return Interval(x, x + 10); });

        IntervalSet<T, IntervalSetStorage::kVector> set(view);

        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(
                        Interval(0, 10), Interval(20, 30), Interval(40, 50)));
    }

    TYPED_TEST(IntervalSetRangesTest, InsertRange)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;

        IntervalSet<T, IntervalSetStorage::kVector> set;
        set.Insert(Interval(0, 10));

        std::vector<Interval> more = {Interval(10, 20), Interval(100, 110)};

        set.InsertRange(more);

        // [0, 10) + [10, 20) merges to [0, 20)
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(
                        Interval(0, 20), Interval(100, 110)));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, Operators)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;

        IntervalSet<T> a;
        a.Insert(Interval(0, 10));
        IntervalSet<T> b;
        b.Insert(Interval(5, 15));

        // Operator + (Union)
        // [0, 10) + [5, 15) = [0, 15)
        IntervalSet<T> c = a + b;
        EXPECT_THAT(GetIntervals(c), ::testing::ElementsAre(Interval(0, 15)));

        // Operator +=
        IntervalSet<T> d = a;
        d += b;
        EXPECT_EQ(c, d);

        // Operator - (Difference)
        // [0, 10) - [5, 15) = [0, 5)
        IntervalSet<T> e = a - b;
        EXPECT_THAT(GetIntervals(e), ::testing::ElementsAre(Interval(0, 5)));

        // Operator -=
        IntervalSet<T> f = a;
        f -= b;
        EXPECT_EQ(e, f);

        // Operator & (Intersection)
        // [0, 10) & [5, 15) = [5, 10)
        IntervalSet<T> g = a & b;
        EXPECT_THAT(GetIntervals(g), ::testing::ElementsAre(Interval(5, 10)));

        // Operator &=
        IntervalSet<T> h = a;
        h &= b;
        EXPECT_EQ(g, h);
    }

    TYPED_TEST(IntervalSetVectorTypedTest, Equality)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;

        IntervalSet<T> a;
        a.Insert(Interval(0, 10));
        a.Insert(Interval(20, 30));
        IntervalSet<T> b;
        b.Insert(Interval(20, 30));
        b.Insert(Interval(0, 10)); // Insert order diff
        IntervalSet<T> c;
        c.Insert(Interval(0, 10));

        EXPECT_TRUE(a == b);
        EXPECT_FALSE(a == c);
        EXPECT_FALSE(a != b);
        EXPECT_TRUE(a != c);
    }

    TYPED_TEST(IntervalSetVectorTypedTest, ReverseIterator)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;

        IntervalSet<T> set;
        set.Insert(Interval(0, 10));
        set.Insert(Interval(20, 30));
        set.Insert(Interval(40, 50));

        std::vector<Interval> reversed;
        for (auto it = set.rbegin(); it != set.rend(); ++it)
        {
            reversed.push_back(*it);
        }

        EXPECT_THAT(reversed, ::testing::ElementsAre(
                        Interval(40, 50), Interval(20, 30), Interval(0, 10)));
    }

    class IntervalSetEpsilonTest : public ::testing::Test
    {
    };

    TEST_F(IntervalSetEpsilonTest, InsertWithTolerance)
    {
        // Test specifically for floats where strict adjacency usually fails
        using Interval = ClosedOpenInterval<float>;
        IntervalSet<float> set;

        set.Insert(Interval(0.0f, 1.0f));

        // Gap is 0.1. Epsilon is 0.05. Should NOT merge.
        set.Insert(Interval(1.1f, 2.0f), 0.05f);
        EXPECT_EQ(set.size(), 2);

        // Gap is 0.1. Epsilon is 0.15. Should merge.
        // [0, 1) and [1.1, 2.0) with eps 0.15 treats gap 0.1 as adjacent
        set.clear();
        set.Insert(Interval(0.0f, 1.0f));
        set.Insert(Interval(1.1f, 2.0f), 0.15f);

        EXPECT_EQ(set.size(), 1);
        // Resulting interval should cover the gap -> [0.0, 2.0)
        const auto it = set.begin();
        EXPECT_FLOAT_EQ(it->GetStart(), 0.0f);
        EXPECT_FLOAT_EQ(it->GetEnd(), 2.0f);
    }

    TEST_F(IntervalSetEpsilonTest, EraseWithTolerance)
    {
        using Interval = ClosedOpenInterval<double>;
        IntervalSet<double> set;

        set.Insert(Interval(0.0, 10.0));

        // Strictly, [10.01, 11.0] does not intersect [0, 10].
        // With epsilon 0.02, it should be considered touching/intersecting logic depending on implementation.
        // The implementation sub_with_floor(start, eps) expands the search window.

        // Let's try to erase a "point" roughly.
        // Interval [5.0, 5.0000001) might be empty/ignored,
        // but Interval(5, 6) strictly intersects.

        // Scenario: Two intervals separated by small gap, erase bridging them.
        set.clear();
        set.Insert(Interval(0.0, 1.0));
        set.Insert(Interval(1.001, 2.0));

        // Verify they are separate initially
        EXPECT_EQ(set.size(), 2);

        // Erase the gap with tolerance.
        // Erase interval is [0.9, 1.1].
        // With epsilon 0, it touches 0.9-1.0 (overlaps [0,1)) and 1.001-1.1 (overlaps [1.001, 2)).
        set.Erase(Interval(0.9, 1.1));

        EXPECT_EQ(set.size(), 2);
        EXPECT_DOUBLE_EQ(set.begin()->GetEnd(), 0.9);
        EXPECT_DOUBLE_EQ(std::next(set.begin())->GetStart(), 1.1);
    }

    TEST_F(IntervalSetEpsilonTest, ContainsValueWithTolerance)
    {
        IntervalSet<float> set;
        set.Insert(ClosedOpenInterval<float>(10.0f, 20.0f));

        // Strictly 9.9 is outside.
        EXPECT_FALSE(set.Contains(9.9f));

        // With epsilon 0.2, 9.9 is close enough to 10.0?
        // Logic check: sub_with_floor(9.9, 0.2) -> 9.7.
        // Candidate search finds [10, 20).
        // Interval::Contains(val, eps) -> 10.0 <= 9.9 + 0.2? Yes (10 <= 10.1).
        EXPECT_TRUE(set.Contains(9.9f, 0.2f));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, EngulfingMerge)
    {
        // Create many small fragmented intervals
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T> set;

        for (int i = 0; i < 100; i += 2)
        {
            set.Insert(Interval(static_cast<T>(i), static_cast<T>(i + 1)));
        }
        EXPECT_EQ(set.size(), 50);

        // Insert one giant interval that covers them all plus gaps
        set.Insert(Interval(0, 100));
        EXPECT_THAT(GetIntervals(set), ::testing::ElementsAre(Interval(0, 100)));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, EngulfingErase)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T> set;
        set.Insert(Interval(0, 100));

        // Punch holes
        for (int i = 10; i < 90; i += 10)
        {
            set.Erase(Interval(static_cast<T>(i), static_cast<T>(i + 1)));
        }
        // We removed [10,11), [20,21)... [80,81)
        // 8 holes created. Original 1 interval becomes 9 intervals.
        EXPECT_EQ(set.size(), 9);
    }

    TYPED_TEST(IntervalSetVectorTypedTest, MoveConstruction)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;

        IntervalSet<T, IntervalSetStorage::kVector> source;
        source.Insert(Interval(static_cast<T>(0), static_cast<T>(10)));
        source.Insert(Interval(static_cast<T>(20), static_cast<T>(30)));

        // Verify source state before move
        ASSERT_FALSE(source.IsEmpty());

        // Move Construct
        IntervalSet<T, IntervalSetStorage::kVector> dest(std::move(source));

        // Source should be empty (valid but unspecified state, usually empty for vector)
        EXPECT_TRUE(source.IsEmpty());
        EXPECT_EQ(dest.size(), 2);
        EXPECT_THAT(GetIntervals(dest), ::testing::ElementsAre(
                        Interval(static_cast<T>(0), static_cast<T>(10)),
                        Interval(static_cast<T>(20), static_cast<T>(30))
                    ));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, MoveAssignment)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;

        IntervalSet<T, IntervalSetStorage::kVector> source;
        source.Insert(Interval(static_cast<T>(0), static_cast<T>(10)));

        IntervalSet<T, IntervalSetStorage::kVector> dest;
        dest.Insert(Interval(static_cast<T>(100), static_cast<T>(110)));

        // Move Assign
        dest = std::move(source);

        EXPECT_TRUE(source.IsEmpty());
        EXPECT_EQ(dest.size(), 1);
        EXPECT_THAT(GetIntervals(dest), ::testing::ElementsAre(
                        Interval(static_cast<T>(0), static_cast<T>(10))
                    ));
    }

    TYPED_TEST(IntervalSetVectorTypedTest, SelfUnion)
    {
        // A + A = A
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T> set;
        set.Insert(Interval(0, 10));
        set.Insert(Interval(20, 30));

        // Copy for verification
        IntervalSet<T> original = set;

        set += set;

        EXPECT_EQ(set.size(), 2);
        EXPECT_EQ(set, original);
    }

    TYPED_TEST(IntervalSetVectorTypedTest, SelfIntersection)
    {
        // A & A = A
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T> set;
        set.Insert(Interval(0, 10));
        set.Insert(Interval(20, 30));

        IntervalSet<T> original = set;

        set &= set;

        EXPECT_EQ(set, original);
    }

    TYPED_TEST(IntervalSetVectorTypedTest, SelfDifference)
    {
        // A - A = Empty
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T> set;
        set.Insert(Interval(0, 10));
        set.Insert(Interval(20, 30));

        set -= set;

        EXPECT_TRUE(set.IsEmpty());
        EXPECT_EQ(set.size(), 0);
    }

    TYPED_TEST(IntervalSetVectorTypedTest, TheZipperMerge)
    {
        // "The Zipper": Create high fragmentation, then fill the gaps.
        // Step 1: [0,1), [2,3), [4,5) ...
        // Step 2: Insert [1,2), [3,4) ...
        // Result should be one massive interval [0, N)

        //

        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T, IntervalSetStorage::kVector> set;

        // Use a smaller N for slower debug builds, larger for release
        const T n = static_cast<T>(1000);

        // 1. Create fragmentation
        for (int i = 0; i < n; i += 2)
        {
            set.Insert(Interval(static_cast<T>(i), static_cast<T>(i + 1)));
        }
        // Should have N/2 intervals
        ASSERT_EQ(set.size(), static_cast<size_t>(n / 2));

        // 2. Fill the gaps (zipping)
        for (int i = 1; i < n; i += 2)
        {
            set.Insert(Interval(static_cast<T>(i), static_cast<T>(i + 1)));
        }

        // 3. Verification
        ASSERT_EQ(set.size(), 1);
        EXPECT_EQ(set.begin()->GetStart(), static_cast<T>(0));
        EXPECT_EQ(set.begin()->GetEnd(), n);
    }

    TYPED_TEST(IntervalSetVectorTypedTest, NumericLimitsEdges)
    {
        // Validates that we don't overflow when checking adjacency at type boundaries
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;

        if constexpr (std::is_integral_v<T>)
        {
            IntervalSet<T> set;
            T min = std::numeric_limits<T>::min();
            T max = std::numeric_limits<T>::max();

            // Insert interval at absolute minimum
            set.Insert(Interval(min, min + 10));
            EXPECT_TRUE(set.Contains(min));

            // Insert interval at absolute maximum (safe range for closed-open is [max-10, max))
            // Note: ClosedOpenInterval usually asserts start < end.
            // [max, max) is empty. [max-10, max) is valid.
            set.Insert(Interval(max - 10, max));
            EXPECT_TRUE(set.Contains(max - 1));
            EXPECT_FALSE(set.Contains(max)); // 'max' is the open end bound

            EXPECT_EQ(set.size(), 2);
        }
    }

    TYPED_TEST(IntervalSetVectorTypedTest, RandomizedDisjointInvariant)
    {
        // Fuzz-lite: Perform random operations and assert strict invariants after every step.
        // 1. Sorted property
        // 2. Disjoint property (end of A <= start of B)
        // 3. Non-adjacent property (end of A < start of B)

        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;
        IntervalSet<T> set;

        // Random number generator
        std::mt19937 rng(42); // NOLINT(*-msc51-cpp)
        // Limit range to ensure frequent overlaps/merges
        std::uniform_int_distribution<int> dist_val(0, 100);
        std::uniform_int_distribution<int> dist_len(1, 20);
        std::uniform_int_distribution<int> dist_op(0, 1); // 0 = Insert, 1 = Erase

        for (int i = 0; i < 500; ++i)
        {
            T start = static_cast<T>(dist_val(rng));
            T end = start + static_cast<T>(dist_len(rng));
            Interval iv(start, end);

            if (dist_op(rng) == 0)
            {
                set.Insert(iv);
            }
            else
            {
                set.Erase(iv);
            }

            // Invariant Check
            if (set.size() > 1)
            {
                auto it = set.begin();
                auto next_it = std::next(it);
                while (next_it != set.end())
                {
                    // Must be sorted
                    EXPECT_LT(it->GetStart(), next_it->GetStart()) << "Unsorted intervals at step " << i;
                    // Must be disjoint AND non-adjacent
                    // (If they were adjacent [0,1) and [1,2), they should have merged)
                    EXPECT_LT(it->GetEnd(), next_it->GetStart())
                        << "Adjacent or overlapping intervals found at step " << i;

                    it = next_it;
                    ++next_it;
                }
            }
        }
    }

    TEST(IntervalSetEdgeCases, UnsignedIntersectionGapMerge)
    {
        // Ensure Insert merges intervals across a gap smaller than epsilon.
        // We use values > epsilon to ensure we test the merge logic itself,
        // avoiding potential unsigned underflow in the underlying Interval class
        // (e.g., if Interval::Intersects does 'start - eps').

        using Interval = ClosedOpenInterval<size_t>;
        IntervalSet<size_t> set;

        // Existing interval: [110, 120)
        set.Insert(Interval(110, 120));

        // New interval: [100, 108).
        // Gap is: 110 - 108 = 2.
        // Epsilon is: 6.
        // Since Gap (2) <= Epsilon (6), they should merge.
        // Note: Length (8) > Epsilon (6), so IsEmpty(eps) check passes.
        set.Insert(Interval(100, 108), 6);

        // Result: [100, 108) + [110, 120) -> [100, 120)
        ASSERT_FALSE(set.IsEmpty());
        EXPECT_EQ(set.size(), 1);
        EXPECT_EQ(set.begin()->GetStart(), 100);
        EXPECT_EQ(set.begin()->GetEnd(), 120);
    }
}
