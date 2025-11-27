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
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
// LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
// OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
// WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

#include <functional>
#include "basalt/math/interval.h"
#include "gtest/gtest.h"
#include "absl/hash/hash.h"
#include "absl/strings/str_format.h"
#include "absl/container/btree_set.h"

namespace bslt::test
{
    static_assert(Interval<ClosedOpenInterval<int32_t>>,
                  "ClosedOpenInterval<int32_t> should satisfy the Interval concept");
    static_assert(Interval<ClosedOpenInterval<double>>,
                  "ClosedOpenInterval<double> should satisfy the Interval concept");

    static_assert(Interval<OpenClosedInterval<int32_t>>,
                  "OpenClosedInterval<int32_t> should satisfy the Interval concept");
    static_assert(Interval<OpenClosedInterval<double>>,
                  "OpenClosedInterval<double> should satisfy the Interval concept");
    static_assert(Interval<ClosedInterval<int32_t>>, "ClosedInterval<int32_t> should satisfy the Interval concept");
    static_assert(Interval<ClosedInterval<double>>, "ClosedInterval<double> should satisfy the Interval concept");
    static_assert(Interval<OpenInterval<int32_t>>, "OpenInterval<int32_t> should satisfy the Interval concept");
    static_assert(Interval<OpenInterval<double>>, "OpenInterval<double> should satisfy the Interval concept");

    using TestTypes = ::testing::Types<int, float, double, size_t>;

    template <typename T>
    class ClosedOpenIntervalTypedTest : public ::testing::Test
    {
    };

    // ReSharper disable once CppDFATimeOver
    TYPED_TEST_SUITE(ClosedOpenIntervalTypedTest, TestTypes);

    TYPED_TEST(ClosedOpenIntervalTypedTest, WorksForBTreeSet)
    {
        using T = TypeParam;
        using Interval = ClosedOpenInterval<T>;

        struct IntervalCompare
        {
            bool operator()(Interval const& a, Interval const& b) const noexcept
            {
                if (a.GetStart() < b.GetStart())
                {
                    return true;
                }
                if (b.GetStart() < a.GetStart())
                {
                    return false;
                }
                return a.GetEnd() < b.GetEnd();
            }
        };

        absl::btree_set<Interval, IntervalCompare> s;
        s.insert(Interval(T{10}, T{20}));
        s.insert(Interval(T{0}, T{10}));
        s.insert(Interval(T{5}, T{15}));
        s.insert(Interval(T{0}, T{10})); // duplicate insert

        EXPECT_EQ(s.size(), static_cast<size_t>(3));

        auto it = s.begin();
        ASSERT_NE(it, s.end());
        EXPECT_EQ(it->GetStart(), T{0});
        EXPECT_EQ(it->GetEnd(), T{10});
        ++it;
        ASSERT_NE(it, s.end());
        EXPECT_EQ(it->GetStart(), T{5});
        EXPECT_EQ(it->GetEnd(), T{15});
        ++it;
        ASSERT_NE(it, s.end());
        EXPECT_EQ(it->GetStart(), T{10});
        EXPECT_EQ(it->GetEnd(), T{20});
        ++it;
        EXPECT_EQ(it, s.end());
    }

    TYPED_TEST(ClosedOpenIntervalTypedTest, ConstructionAndGetters)
    {
        using T = TypeParam;
        const T start{0};
        const T end{10};
        const ClosedOpenInterval<T> interval(start, end);

        EXPECT_EQ(interval.GetStart(), start);
        EXPECT_EQ(interval.GetEnd(), end);
    }

    TYPED_TEST(ClosedOpenIntervalTypedTest, IsEmpty)
    {
        using T = TypeParam;
        // [5, 5) is empty
        const ClosedOpenInterval<T> empty_interval(T{5}, T{5});
        const ClosedOpenInterval<T> normal_interval(T{5}, T{10});

        EXPECT_TRUE(empty_interval.IsEmpty());
        EXPECT_FALSE(normal_interval.IsEmpty());
    }

    TYPED_TEST(ClosedOpenIntervalTypedTest, Length)
    {
        using T = TypeParam;
        const ClosedOpenInterval<T> interval(T{5}, T{15});
        const ClosedOpenInterval<T> empty_interval(T{5}, T{5});

        EXPECT_EQ(interval.Length(), T{10});
        EXPECT_EQ(empty_interval.Length(), T{0});

        if constexpr (std::is_signed_v<T>)
        {
            const ClosedOpenInterval<T> negative_interval(T{-10}, T{0});
            EXPECT_EQ(negative_interval.Length(), T{10});
        }
    }

    TYPED_TEST(ClosedOpenIntervalTypedTest, Midpoint)
    {
        using T = TypeParam;
        const ClosedOpenInterval<T> interval_even(T{0}, T{10});
        const ClosedOpenInterval<T> interval_odd(T{0}, T{5});

        if constexpr (std::is_floating_point_v<T>)
        {
            EXPECT_EQ(interval_even.Midpoint(), T{5.0});
            EXPECT_EQ(interval_odd.Midpoint(), T{2.5});
            if constexpr (std::is_signed_v<T>)
            {
                const ClosedOpenInterval<T> interval_negative(T{-10}, T{0});
                EXPECT_EQ(interval_negative.Midpoint(), T{-5.0});
            }
        }
        else
        {
            // Integer division behavior
            EXPECT_EQ(interval_even.Midpoint(), T{5});
            EXPECT_EQ(interval_odd.Midpoint(), T{2}); // 0 + (5-0)/2 = 2
            if constexpr (std::is_signed_v<T>)
            {
                const ClosedOpenInterval<T> interval_negative(T{-10}, T{0});
                EXPECT_EQ(interval_negative.Midpoint(), T{-5}); // -10 + (0 - (-10))/2 = -5
            }
        }
    }

    TYPED_TEST(ClosedOpenIntervalTypedTest, ContainsValue)
    {
        using T = TypeParam;
        const ClosedOpenInterval<T> interval(T{0}, T{10}); // [0, 10)

        // Start is inclusive
        EXPECT_TRUE(interval.Contains(T{0}));
        // Middle
        EXPECT_TRUE(interval.Contains(T{5}));

        // End is exclusive
        EXPECT_FALSE(interval.Contains(T{10}));

        // Values just inside/outside the end
        if constexpr (std::is_floating_point_v<T>)
        {
            EXPECT_TRUE(interval.Contains(T{9.9999}));
        }
        else
        {
            EXPECT_TRUE(interval.Contains(T{9}));
        }

        // Outside
        if constexpr (std::is_signed_v<T>)
        {
            EXPECT_FALSE(interval.Contains(T{-1}));
        }
        EXPECT_FALSE(interval.Contains(T{11}));
    }

    TYPED_TEST(ClosedOpenIntervalTypedTest, ContainsInterval)
    {
        using T = TypeParam;
        const ClosedOpenInterval<T> i(T{0}, T{10});

        EXPECT_TRUE(i.ContainsInterval(ClosedOpenInterval<T>(T{2}, T{8}))); // Fully inside
        EXPECT_TRUE(i.ContainsInterval(ClosedOpenInterval<T>(T{0}, T{10}))); // Identical
        EXPECT_TRUE(i.ContainsInterval(ClosedOpenInterval<T>(T{0}, T{5}))); // Touching start
        EXPECT_TRUE(i.ContainsInterval(ClosedOpenInterval<T>(T{5}, T{10}))); // Touching end
        EXPECT_TRUE(i.ContainsInterval(ClosedOpenInterval<T>(T{5}, T{5}))); // Empty inside

        if constexpr (std::is_signed_v<T>)
        {
            EXPECT_FALSE(i.ContainsInterval(ClosedOpenInterval<T>(T{-1}, T{5}))); // Overlap start
            EXPECT_FALSE(i.ContainsInterval(ClosedOpenInterval<T>(T{-1}, T{11}))); // Superset
        }
        EXPECT_FALSE(i.ContainsInterval(ClosedOpenInterval<T>(T{5}, T{11}))); // Overlap end
    }

    TYPED_TEST(ClosedOpenIntervalTypedTest, Intersects)
    {
        using T = TypeParam;
        const ClosedOpenInterval<T> i(T{5}, T{15}); // [5, 15)

        EXPECT_TRUE(i.Intersects(ClosedOpenInterval<T>(T{0}, T{10}))); // Overlap start [5, 10)
        EXPECT_TRUE(i.Intersects(ClosedOpenInterval<T>(T{10}, T{20}))); // Overlap end [10, 15)
        EXPECT_TRUE(i.Intersects(ClosedOpenInterval<T>(T{7}, T{12}))); // Subset
        EXPECT_TRUE(i.Intersects(ClosedOpenInterval<T>(T{0}, T{20}))); // Superset

        // Adjacent is NOT intersecting for [start, end)
        EXPECT_FALSE(i.Intersects(ClosedOpenInterval<T>(T{0}, T{5}))); // Adjacent start, intersection is [5, 5) (empty)
        EXPECT_FALSE(i.Intersects(ClosedOpenInterval<T>(T{15}, T{20})));
        // Adjacent end, intersection is [15, 15) (empty)

        // Disjoint
        EXPECT_FALSE(i.Intersects(ClosedOpenInterval<T>(T{0}, T{4})));
        EXPECT_FALSE(i.Intersects(ClosedOpenInterval<T>(T{16}, T{20})));
    }

    TYPED_TEST(ClosedOpenIntervalTypedTest, Adjacent)
    {
        using T = TypeParam;
        const ClosedOpenInterval<T> i(T{5}, T{15});

        // Adjacent (touching)
        EXPECT_TRUE(i.Adjacent(ClosedOpenInterval<T>(T{0}, T{5})));
        EXPECT_TRUE(i.Adjacent(ClosedOpenInterval<T>(T{15}, T{20})));

        // Overlapping is NOT adjacent
        EXPECT_FALSE(i.Adjacent(ClosedOpenInterval<T>(T{0}, T{6})));
        EXPECT_FALSE(i.Adjacent(ClosedOpenInterval<T>(T{14}, T{20})));
        EXPECT_FALSE(i.Adjacent(ClosedOpenInterval<T>(T{7}, T{12})));

        // Disjoint is NOT adjacent
        EXPECT_FALSE(i.Adjacent(ClosedOpenInterval<T>(T{0}, T{4})));
        EXPECT_FALSE(i.Adjacent(ClosedOpenInterval<T>(T{16}, T{20})));
    }

    TYPED_TEST(ClosedOpenIntervalTypedTest, IntersectsOrAdjacent)
    {
        using T = TypeParam;
        const ClosedOpenInterval<T> i(T{5}, T{15});

        // Overlapping
        EXPECT_TRUE(i.IntersectsOrAdjacent(ClosedOpenInterval<T>(T{0}, T{10})));
        EXPECT_TRUE(i.IntersectsOrAdjacent(ClosedOpenInterval<T>(T{10}, T{20})));

        // Adjacent
        EXPECT_TRUE(i.IntersectsOrAdjacent(ClosedOpenInterval<T>(T{0}, T{5})));
        EXPECT_TRUE(i.IntersectsOrAdjacent(ClosedOpenInterval<T>(T{15}, T{20})));

        // Disjoint
        EXPECT_FALSE(i.IntersectsOrAdjacent(ClosedOpenInterval<T>(T{0}, T{4})));
        EXPECT_FALSE(i.IntersectsOrAdjacent(ClosedOpenInterval<T>(T{16}, T{20})));
    }

    TYPED_TEST(ClosedOpenIntervalTypedTest, DistanceTo)
    {
        using T = TypeParam;
        const ClosedOpenInterval<T> i(T{10}, T{20});

        // Overlapping
        EXPECT_EQ(i.DistanceTo(ClosedOpenInterval<T>(T{15}, T{25})), T{0});
        // Adjacent
        EXPECT_EQ(i.DistanceTo(ClosedOpenInterval<T>(T{20}, T{25})), T{0});
        EXPECT_EQ(i.DistanceTo(ClosedOpenInterval<T>(T{5}, T{10})), T{0});

        // Disjoint
        EXPECT_EQ(i.DistanceTo(ClosedOpenInterval<T>(T{22}, T{25})), T{2});
        EXPECT_EQ(i.DistanceTo(ClosedOpenInterval<T>(T{5}, T{8})), T{2});
    }

    TYPED_TEST(ClosedOpenIntervalTypedTest, Intersection)
    {
        using T = TypeParam;
        const ClosedOpenInterval<T> i(T{10}, T{20});

        // Overlap start
        auto res1 = i.Intersection(ClosedOpenInterval<T>(T{5}, T{15}));
        EXPECT_TRUE(res1.has_value());
        EXPECT_EQ(res1.value(), ClosedOpenInterval<T>(T{10}, T{15}));

        // Overlap end
        auto res2 = i.Intersection(ClosedOpenInterval<T>(T{15}, T{25}));
        EXPECT_TRUE(res2.has_value());
        EXPECT_EQ(res2.value(), ClosedOpenInterval<T>(T{15}, T{20}));

        // Subset
        auto res3 = i.Intersection(ClosedOpenInterval<T>(T{12}, T{18}));
        EXPECT_TRUE(res3.has_value());
        EXPECT_EQ(res3.value(), ClosedOpenInterval<T>(T{12}, T{18}));

        // Superset
        auto res4 = i.Intersection(ClosedOpenInterval<T>(T{5}, T{25}));
        EXPECT_TRUE(res4.has_value());
        EXPECT_EQ(res4.value(), ClosedOpenInterval<T>(T{10}, T{20}));

        // Adjacent
        auto res5 = i.Intersection(ClosedOpenInterval<T>(T{20}, T{25}));
        EXPECT_FALSE(res5.has_value());

        // Disjoint
        auto res6 = i.Intersection(ClosedOpenInterval<T>(T{0}, T{5}));
        EXPECT_FALSE(res6.has_value());
    }

    TYPED_TEST(ClosedOpenIntervalTypedTest, Clamp)
    {
        using T = TypeParam;
        // Clamp is synonymous with Intersection for this class.
        const ClosedOpenInterval<T> i(T{10}, T{20});
        const ClosedOpenInterval<T> boundary(T{15}, T{25});

        auto res = i.Clamp(boundary);
        EXPECT_TRUE(res.has_value());
        EXPECT_EQ(res.value(), ClosedOpenInterval<T>(T{15}, T{20}));

        auto res2 = boundary.Clamp(i);
        EXPECT_TRUE(res2.has_value());
        EXPECT_EQ(res2.value(), ClosedOpenInterval<T>(T{15}, T{20}));

        const ClosedOpenInterval<T> disjoint_boundary(T{30}, T{40});
        auto res3 = i.Clamp(disjoint_boundary);
        EXPECT_FALSE(res3.has_value());
    }

    TYPED_TEST(ClosedOpenIntervalTypedTest, Merge)
    {
        using T = TypeParam;
        const ClosedOpenInterval<T> i(T{10}, T{20});

        // Overlapping
        auto res1 = i.Merge(ClosedOpenInterval<T>(T{15}, T{25}));
        EXPECT_TRUE(res1.has_value());
        EXPECT_EQ(res1.value(), ClosedOpenInterval<T>(T{10}, T{25}));

        // Adjacent
        auto res2 = i.Merge(ClosedOpenInterval<T>(T{20}, T{25}));
        EXPECT_TRUE(res2.has_value());
        EXPECT_EQ(res2.value(), ClosedOpenInterval<T>(T{10}, T{25}));

        auto res3 = i.Merge(ClosedOpenInterval<T>(T{5}, T{10}));
        EXPECT_TRUE(res3.has_value());
        EXPECT_EQ(res3.value(), ClosedOpenInterval<T>(T{5}, T{20}));

        // Subset
        auto res4 = i.Merge(ClosedOpenInterval<T>(T{12}, T{18}));
        EXPECT_TRUE(res4.has_value());
        EXPECT_EQ(res4.value(), ClosedOpenInterval<T>(T{10}, T{20}));

        // Disjoint
        auto res5 = i.Merge(ClosedOpenInterval<T>(T{21}, T{25}));
        EXPECT_FALSE(res5.has_value());
    }

    TYPED_TEST(ClosedOpenIntervalTypedTest, Combine)
    {
        using T = TypeParam;
        const ClosedOpenInterval<T> i(T{10}, T{20});

        // Overlapping
        auto res1 = i.Combine(ClosedOpenInterval<T>(T{15}, T{25}));
        EXPECT_EQ(res1, ClosedOpenInterval<T>(T{10}, T{25}));

        // Adjacent
        auto res2 = i.Combine(ClosedOpenInterval<T>(T{20}, T{25}));
        EXPECT_EQ(res2, ClosedOpenInterval<T>(T{10}, T{25}));

        // Disjoint (Key difference from Merge)
        auto res3 = i.Combine(ClosedOpenInterval<T>(T{21}, T{25}));
        EXPECT_EQ(res3, ClosedOpenInterval<T>(T{10}, T{25}));

        auto res4 = i.Combine(ClosedOpenInterval<T>(T{0}, T{5}));
        EXPECT_EQ(res4, ClosedOpenInterval<T>(T{0}, T{20}));
    }

    // =========================================================================
    // OpenClosedInterval (start, end]
    // =========================================================================

    template <typename T>
    class OpenClosedIntervalTypedTest : public ::testing::Test
    {
    };

    TYPED_TEST_SUITE(OpenClosedIntervalTypedTest, TestTypes);

    TYPED_TEST(OpenClosedIntervalTypedTest, ConstructionAndGetters)
    {
        using T = TypeParam;
        const T start{0};
        const T end{10};
        const OpenClosedInterval<T> interval(start, end);

        EXPECT_EQ(interval.GetStart(), start);
        EXPECT_EQ(interval.GetEnd(), end);
    }

    TYPED_TEST(OpenClosedIntervalTypedTest, IsEmpty)
    {
        using T = TypeParam;
        // (5, 5] is empty
        const OpenClosedInterval<T> empty_interval(T{5}, T{5});
        const OpenClosedInterval<T> normal_interval(T{5}, T{10});

        EXPECT_TRUE(empty_interval.IsEmpty());
        EXPECT_FALSE(normal_interval.IsEmpty());
    }

    TYPED_TEST(OpenClosedIntervalTypedTest, Length)
    {
        using T = TypeParam;
        const OpenClosedInterval<T> interval(T{5}, T{15});
        const OpenClosedInterval<T> empty_interval(T{5}, T{5});

        EXPECT_EQ(interval.Length(), T{10});
        EXPECT_EQ(empty_interval.Length(), T{0});

        if constexpr (std::is_signed_v<T>)
        {
            const OpenClosedInterval<T> negative_interval(T{-10}, T{0});
            EXPECT_EQ(negative_interval.Length(), T{10});
        }
    }

    TYPED_TEST(OpenClosedIntervalTypedTest, Midpoint)
    {
        using T = TypeParam;
        const OpenClosedInterval<T> interval_even(T{0}, T{10});
        const OpenClosedInterval<T> interval_odd(T{0}, T{5});

        if constexpr (std::is_floating_point_v<T>)
        {
            EXPECT_EQ(interval_even.Midpoint(), T{5.0});
            EXPECT_EQ(interval_odd.Midpoint(), T{2.5});
            if constexpr (std::is_signed_v<T>)
            {
                const OpenClosedInterval<T> interval_negative(T{-10}, T{0});
                EXPECT_EQ(interval_negative.Midpoint(), T{-5.0});
            }
        }
        else
        {
            // Integer division behavior
            EXPECT_EQ(interval_even.Midpoint(), T{5});
            EXPECT_EQ(interval_odd.Midpoint(), T{2}); // 0 + (5-0)/2 = 2
            if constexpr (std::is_signed_v<T>)
            {
                const OpenClosedInterval<T> interval_negative(T{-10}, T{0});
                EXPECT_EQ(interval_negative.Midpoint(), T{-5}); // -10 + (0 - (-10))/2 = -5
            }
        }
    }

    TYPED_TEST(OpenClosedIntervalTypedTest, ContainsValue)
    {
        using T = TypeParam;
        const OpenClosedInterval<T> interval(T{0}, T{10}); // (0, 10]

        // Start is exclusive
        EXPECT_FALSE(interval.Contains(T{0}));
        // Middle
        EXPECT_TRUE(interval.Contains(T{5}));

        // End is inclusive
        EXPECT_TRUE(interval.Contains(T{10}));

        // Values just inside/outside the end
        if constexpr (std::is_floating_point_v<T>)
        {
            EXPECT_TRUE(interval.Contains(T{0.0001}));
        }
        else
        {
            EXPECT_TRUE(interval.Contains(T{1}));
        }

        // Outside
        if constexpr (std::is_signed_v<T>)
        {
            EXPECT_FALSE(interval.Contains(T{-1}));
        }
        EXPECT_FALSE(interval.Contains(T{11}));
    }

    TYPED_TEST(OpenClosedIntervalTypedTest, ContainsInterval)
    {
        using T = TypeParam;
        const OpenClosedInterval<T> i(T{0}, T{10});

        EXPECT_TRUE(i.ContainsInterval(OpenClosedInterval<T>(T{2}, T{8}))); // Fully inside
        EXPECT_TRUE(i.ContainsInterval(OpenClosedInterval<T>(T{0}, T{10}))); // Identical
        EXPECT_TRUE(i.ContainsInterval(OpenClosedInterval<T>(T{0}, T{5}))); // Touching start
        EXPECT_TRUE(i.ContainsInterval(OpenClosedInterval<T>(T{5}, T{10}))); // Touching end
        EXPECT_TRUE(i.ContainsInterval(OpenClosedInterval<T>(T{5}, T{5}))); // Empty inside

        if constexpr (std::is_signed_v<T>)
        {
            EXPECT_FALSE(i.ContainsInterval(OpenClosedInterval<T>(T{-1}, T{5}))); // Overlap start
            EXPECT_FALSE(i.ContainsInterval(OpenClosedInterval<T>(T{-1}, T{11}))); // Superset
        }
        EXPECT_FALSE(i.ContainsInterval(OpenClosedInterval<T>(T{5}, T{11}))); // Overlap end
    }

    TYPED_TEST(OpenClosedIntervalTypedTest, Intersects)
    {
        using T = TypeParam;
        const OpenClosedInterval<T> i(T{5}, T{15}); // (5, 15]

        EXPECT_TRUE(i.Intersects(OpenClosedInterval<T>(T{0}, T{10}))); // Overlap start (5, 10]
        EXPECT_TRUE(i.Intersects(OpenClosedInterval<T>(T{10}, T{20}))); // Overlap end (10, 15]
        EXPECT_TRUE(i.Intersects(OpenClosedInterval<T>(T{7}, T{12}))); // Subset
        EXPECT_TRUE(i.Intersects(OpenClosedInterval<T>(T{0}, T{20}))); // Superset

        // Adjacent is NOT intersecting for (start, end]
        EXPECT_FALSE(i.Intersects(OpenClosedInterval<T>(T{0}, T{5}))); // Adjacent start, intersection is (5, 5] (empty)
        EXPECT_FALSE(i.Intersects(OpenClosedInterval<T>(T{15}, T{20})));
        // Adjacent end, intersection is (15, 15] (empty)

        // Disjoint
        EXPECT_FALSE(i.Intersects(OpenClosedInterval<T>(T{0}, T{4})));
        EXPECT_FALSE(i.Intersects(OpenClosedInterval<T>(T{16}, T{20})));
    }

    TYPED_TEST(OpenClosedIntervalTypedTest, Adjacent)
    {
        using T = TypeParam;
        const OpenClosedInterval<T> i(T{5}, T{15});

        // Adjacent (touching)
        EXPECT_TRUE(i.Adjacent(OpenClosedInterval<T>(T{0}, T{5})));
        EXPECT_TRUE(i.Adjacent(OpenClosedInterval<T>(T{15}, T{20})));

        // Overlapping is NOT adjacent
        EXPECT_FALSE(i.Adjacent(OpenClosedInterval<T>(T{0}, T{6})));
        EXPECT_FALSE(i.Adjacent(OpenClosedInterval<T>(T{14}, T{20})));
        EXPECT_FALSE(i.Adjacent(OpenClosedInterval<T>(T{7}, T{12})));

        // Disjoint is NOT adjacent
        EXPECT_FALSE(i.Adjacent(OpenClosedInterval<T>(T{0}, T{4})));
        EXPECT_FALSE(i.Adjacent(OpenClosedInterval<T>(T{16}, T{20})));
    }

    TYPED_TEST(OpenClosedIntervalTypedTest, IntersectsOrAdjacent)
    {
        using T = TypeParam;
        const OpenClosedInterval<T> i(T{5}, T{15});

        // Overlapping
        EXPECT_TRUE(i.IntersectsOrAdjacent(OpenClosedInterval<T>(T{0}, T{10})));
        EXPECT_TRUE(i.IntersectsOrAdjacent(OpenClosedInterval<T>(T{10}, T{20})));

        // Adjacent
        EXPECT_TRUE(i.IntersectsOrAdjacent(OpenClosedInterval<T>(T{0}, T{5})));
        EXPECT_TRUE(i.IntersectsOrAdjacent(OpenClosedInterval<T>(T{15}, T{20})));

        // Disjoint
        EXPECT_FALSE(i.IntersectsOrAdjacent(OpenClosedInterval<T>(T{0}, T{4})));
        EXPECT_FALSE(i.IntersectsOrAdjacent(OpenClosedInterval<T>(T{16}, T{20})));
    }

    TYPED_TEST(OpenClosedIntervalTypedTest, DistanceTo)
    {
        using T = TypeParam;
        const OpenClosedInterval<T> i(T{10}, T{20});

        // Overlapping
        EXPECT_EQ(i.DistanceTo(OpenClosedInterval<T>(T{15}, T{25})), T{0});
        // Adjacent
        EXPECT_EQ(i.DistanceTo(OpenClosedInterval<T>(T{20}, T{25})), T{0});
        EXPECT_EQ(i.DistanceTo(OpenClosedInterval<T>(T{5}, T{10})), T{0});

        // Disjoint
        EXPECT_EQ(i.DistanceTo(OpenClosedInterval<T>(T{22}, T{25})), T{2});
        EXPECT_EQ(i.DistanceTo(OpenClosedInterval<T>(T{5}, T{8})), T{2});
    }

    TYPED_TEST(OpenClosedIntervalTypedTest, Intersection)
    {
        using T = TypeParam;
        const OpenClosedInterval<T> i(T{10}, T{20});

        // Overlap start
        auto res1 = i.Intersection(OpenClosedInterval<T>(T{5}, T{15}));
        EXPECT_TRUE(res1.has_value());
        EXPECT_EQ(res1.value(), OpenClosedInterval<T>(T{10}, T{15}));

        // Overlap end
        auto res2 = i.Intersection(OpenClosedInterval<T>(T{15}, T{25}));
        EXPECT_TRUE(res2.has_value());
        EXPECT_EQ(res2.value(), OpenClosedInterval<T>(T{15}, T{20}));

        // Subset
        auto res3 = i.Intersection(OpenClosedInterval<T>(T{12}, T{18}));
        EXPECT_TRUE(res3.has_value());
        EXPECT_EQ(res3.value(), OpenClosedInterval<T>(T{12}, T{18}));

        // Superset
        auto res4 = i.Intersection(OpenClosedInterval<T>(T{5}, T{25}));
        EXPECT_TRUE(res4.has_value());
        EXPECT_EQ(res4.value(), OpenClosedInterval<T>(T{10}, T{20}));

        // Adjacent
        auto res5 = i.Intersection(OpenClosedInterval<T>(T{20}, T{25}));
        EXPECT_FALSE(res5.has_value());

        // Disjoint
        auto res6 = i.Intersection(OpenClosedInterval<T>(T{0}, T{5}));
        EXPECT_FALSE(res6.has_value());
    }

    TYPED_TEST(OpenClosedIntervalTypedTest, Clamp)
    {
        using T = TypeParam;
        // Clamp is synonymous with Intersection for this class.
        const OpenClosedInterval<T> i(T{10}, T{20});
        const OpenClosedInterval<T> boundary(T{15}, T{25});

        auto res = i.Clamp(boundary);
        EXPECT_TRUE(res.has_value());
        EXPECT_EQ(res.value(), OpenClosedInterval<T>(T{15}, T{20}));

        auto res2 = boundary.Clamp(i);
        EXPECT_TRUE(res2.has_value());
        EXPECT_EQ(res2.value(), OpenClosedInterval<T>(T{15}, T{20}));

        const OpenClosedInterval<T> disjoint_boundary(T{30}, T{40});
        auto res3 = i.Clamp(disjoint_boundary);
        EXPECT_FALSE(res3.has_value());
    }

    TYPED_TEST(OpenClosedIntervalTypedTest, Merge)
    {
        using T = TypeParam;
        const OpenClosedInterval<T> i(T{10}, T{20});

        // Overlapping
        auto res1 = i.Merge(OpenClosedInterval<T>(T{15}, T{25}));
        EXPECT_TRUE(res1.has_value());
        EXPECT_EQ(res1.value(), OpenClosedInterval<T>(T{10}, T{25}));

        // Adjacent
        auto res2 = i.Merge(OpenClosedInterval<T>(T{20}, T{25}));
        EXPECT_TRUE(res2.has_value());
        EXPECT_EQ(res2.value(), OpenClosedInterval<T>(T{10}, T{25}));

        auto res3 = i.Merge(OpenClosedInterval<T>(T{5}, T{10}));
        EXPECT_TRUE(res3.has_value());
        EXPECT_EQ(res3.value(), OpenClosedInterval<T>(T{5}, T{20}));

        // Subset
        auto res4 = i.Merge(OpenClosedInterval<T>(T{12}, T{18}));
        EXPECT_TRUE(res4.has_value());
        EXPECT_EQ(res4.value(), OpenClosedInterval<T>(T{10}, T{20}));

        // Disjoint
        auto res5 = i.Merge(OpenClosedInterval<T>(T{21}, T{25}));
        EXPECT_FALSE(res5.has_value());
    }

    TYPED_TEST(OpenClosedIntervalTypedTest, Combine)
    {
        using T = TypeParam;
        const OpenClosedInterval<T> i(T{10}, T{20});

        // Overlapping
        auto res1 = i.Combine(OpenClosedInterval<T>(T{15}, T{25}));
        EXPECT_EQ(res1, OpenClosedInterval<T>(T{10}, T{25}));

        // Adjacent
        auto res2 = i.Combine(OpenClosedInterval<T>(T{20}, T{25}));
        EXPECT_EQ(res2, OpenClosedInterval<T>(T{10}, T{25}));

        // Disjoint (Key difference from Merge)
        auto res3 = i.Combine(OpenClosedInterval<T>(T{21}, T{25}));
        EXPECT_EQ(res3, OpenClosedInterval<T>(T{10}, T{25}));

        auto res4 = i.Combine(OpenClosedInterval<T>(T{0}, T{5}));
        EXPECT_EQ(res4, OpenClosedInterval<T>(T{0}, T{20}));
    }

    // =========================================================================
    // ClosedInterval [start, end]
    // =========================================================================

    template <typename T>
    class ClosedIntervalTypedTest : public ::testing::Test
    {
    };

    TYPED_TEST_SUITE(ClosedIntervalTypedTest, TestTypes);

    TYPED_TEST(ClosedIntervalTypedTest, ConstructionAndGetters)
    {
        using T = TypeParam;
        const T start{0};
        const T end{10};
        const ClosedInterval<T> interval(start, end);

        EXPECT_EQ(interval.GetStart(), start);
        EXPECT_EQ(interval.GetEnd(), end);
    }

    TYPED_TEST(ClosedIntervalTypedTest, IsEmpty)
    {
        using T = TypeParam;
        // [5, 5] is NOT empty, it contains one point.
        const ClosedInterval<T> non_empty_point(T{5}, T{5});
        const ClosedInterval<T> normal_interval(T{5}, T{10});

        // An interval like [6, 5] is constructed as [5, 6], so IsEmpty (start > end)
        // should always be false with the current constructor.
        EXPECT_FALSE(non_empty_point.IsEmpty());
        EXPECT_FALSE(normal_interval.IsEmpty());
    }

    TYPED_TEST(ClosedIntervalTypedTest, Length)
    {
        using T = TypeParam;
        const ClosedInterval<T> interval(T{5}, T{15});
        const ClosedInterval<T> point_interval(T{5}, T{5});

        EXPECT_EQ(interval.Length(), T{10});
        EXPECT_EQ(point_interval.Length(), T{0});

        if constexpr (std::is_signed_v<T>)
        {
            const ClosedInterval<T> negative_interval(T{-10}, T{0});
            EXPECT_EQ(negative_interval.Length(), T{10});
        }
    }

    TYPED_TEST(ClosedIntervalTypedTest, Midpoint)
    {
        using T = TypeParam;
        const ClosedInterval<T> interval_even(T{0}, T{10});
        const ClosedInterval<T> interval_odd(T{0}, T{5});

        if constexpr (std::is_floating_point_v<T>)
        {
            EXPECT_EQ(interval_even.Midpoint(), T{5.0});
            EXPECT_EQ(interval_odd.Midpoint(), T{2.5});
            if constexpr (std::is_signed_v<T>)
            {
                const ClosedInterval<T> interval_negative(T{-10}, T{0});
                EXPECT_EQ(interval_negative.Midpoint(), T{-5.0});
            }
        }
        else
        {
            // Integer division behavior
            EXPECT_EQ(interval_even.Midpoint(), T{5});
            EXPECT_EQ(interval_odd.Midpoint(), T{2}); // 0 + (5-0)/2 = 2
            if constexpr (std::is_signed_v<T>)
            {
                const ClosedInterval<T> interval_negative(T{-10}, T{0});
                EXPECT_EQ(interval_negative.Midpoint(), T{-5}); // -10 + (0 - (-10))/2 = -5
            }
        }
    }

    TYPED_TEST(ClosedIntervalTypedTest, ContainsValue)
    {
        using T = TypeParam;
        const ClosedInterval<T> interval(T{0}, T{10}); // [0, 10]

        // Start is inclusive
        EXPECT_TRUE(interval.Contains(T{0}));
        // Middle
        EXPECT_TRUE(interval.Contains(T{5}));

        // End is inclusive
        EXPECT_TRUE(interval.Contains(T{10}));

        // Values just inside/outside the end
        if constexpr (std::is_floating_point_v<T>)
        {
            EXPECT_TRUE(interval.Contains(T{9.9999}));
            EXPECT_FALSE(interval.Contains(T{10.0001}));
        }
        else
        {
            EXPECT_TRUE(interval.Contains(T{9}));
        }

        // Outside
        if constexpr (std::is_signed_v<T>)
        {
            EXPECT_FALSE(interval.Contains(T{-1}));
        }
        EXPECT_FALSE(interval.Contains(T{11}));
    }

    TYPED_TEST(ClosedIntervalTypedTest, ContainsInterval)
    {
        using T = TypeParam;
        const ClosedInterval<T> i(T{0}, T{10});

        EXPECT_TRUE(i.ContainsInterval(ClosedInterval<T>(T{2}, T{8}))); // Fully inside
        EXPECT_TRUE(i.ContainsInterval(ClosedInterval<T>(T{0}, T{10}))); // Identical
        EXPECT_TRUE(i.ContainsInterval(ClosedInterval<T>(T{0}, T{5}))); // Touching start
        EXPECT_TRUE(i.ContainsInterval(ClosedInterval<T>(T{5}, T{10}))); // Touching end
        EXPECT_TRUE(i.ContainsInterval(ClosedInterval<T>(T{5}, T{5}))); // Point inside

        if constexpr (std::is_signed_v<T>)
        {
            EXPECT_FALSE(i.ContainsInterval(ClosedInterval<T>(T{-1}, T{5}))); // Overlap start
            EXPECT_FALSE(i.ContainsInterval(ClosedInterval<T>(T{-1}, T{11}))); // Superset
        }
        EXPECT_FALSE(i.ContainsInterval(ClosedInterval<T>(T{5}, T{11}))); // Overlap end
    }

    TYPED_TEST(ClosedIntervalTypedTest, Intersects)
    {
        using T = TypeParam;
        const ClosedInterval<T> i(T{5}, T{15}); // [5, 15]

        EXPECT_TRUE(i.Intersects(ClosedInterval<T>(T{0}, T{10}))); // Overlap start [5, 10]
        EXPECT_TRUE(i.Intersects(ClosedInterval<T>(T{10}, T{20}))); // Overlap end [10, 15]
        EXPECT_TRUE(i.Intersects(ClosedInterval<T>(T{7}, T{12}))); // Subset
        EXPECT_TRUE(i.Intersects(ClosedInterval<T>(T{0}, T{20}))); // Superset

        // Adjacent IS intersecting for [start, end]
        EXPECT_TRUE(i.Intersects(ClosedInterval<T>(T{0}, T{5}))); // Adjacent start, intersection is [5, 5]
        EXPECT_TRUE(i.Intersects(ClosedInterval<T>(T{15}, T{20}))); // Adjacent end, intersection is [15, 15]

        // Disjoint
        EXPECT_FALSE(i.Intersects(ClosedInterval<T>(T{0}, T{4})));
        EXPECT_FALSE(i.Intersects(ClosedInterval<T>(T{16}, T{20})));
    }

    TYPED_TEST(ClosedIntervalTypedTest, Adjacent)
    {
        using T = TypeParam;
        const ClosedInterval<T> i(T{5}, T{15});

        // Adjacent (touching at a single point)
        // Note: For ClosedInterval, Adjacent and Intersecting-at-one-point are the same.
        EXPECT_TRUE(i.Adjacent(ClosedInterval<T>(T{0}, T{5})));
        EXPECT_TRUE(i.Adjacent(ClosedInterval<T>(T{15}, T{20})));

        // Overlapping by more than one point is NOT adjacent
        EXPECT_FALSE(i.Adjacent(ClosedInterval<T>(T{0}, T{6})));
        EXPECT_FALSE(i.Adjacent(ClosedInterval<T>(T{14}, T{20})));
        EXPECT_FALSE(i.Adjacent(ClosedInterval<T>(T{7}, T{12})));

        // Disjoint is NOT adjacent
        EXPECT_FALSE(i.Adjacent(ClosedInterval<T>(T{0}, T{4})));
        EXPECT_FALSE(i.Adjacent(ClosedInterval<T>(T{16}, T{20})));
    }

    TYPED_TEST(ClosedIntervalTypedTest, IntersectsOrAdjacent)
    {
        using T = TypeParam;
        const ClosedInterval<T> i(T{5}, T{15});
        // This is identical to Intersects() for ClosedInterval

        // Overlapping
        EXPECT_TRUE(i.IntersectsOrAdjacent(ClosedInterval<T>(T{0}, T{10})));
        EXPECT_TRUE(i.IntersectsOrAdjacent(ClosedInterval<T>(T{10}, T{20})));

        // Adjacent
        EXPECT_TRUE(i.IntersectsOrAdjacent(ClosedInterval<T>(T{0}, T{5})));
        EXPECT_TRUE(i.IntersectsOrAdjacent(ClosedInterval<T>(T{15}, T{20})));

        // Disjoint
        EXPECT_FALSE(i.IntersectsOrAdjacent(ClosedInterval<T>(T{0}, T{4})));
        EXPECT_FALSE(i.IntersectsOrAdjacent(ClosedInterval<T>(T{16}, T{20})));
    }

    TYPED_TEST(ClosedIntervalTypedTest, DistanceTo)
    {
        using T = TypeParam;
        const ClosedInterval<T> i(T{10}, T{20});

        // Overlapping
        EXPECT_EQ(i.DistanceTo(ClosedInterval<T>(T{15}, T{25})), T{0});
        // Adjacent
        EXPECT_EQ(i.DistanceTo(ClosedInterval<T>(T{20}, T{25})), T{0});
        EXPECT_EQ(i.DistanceTo(ClosedInterval<T>(T{5}, T{10})), T{0});

        // Disjoint
        EXPECT_EQ(i.DistanceTo(ClosedInterval<T>(T{22}, T{25})), T{2});
        EXPECT_EQ(i.DistanceTo(ClosedInterval<T>(T{5}, T{8})), T{2});
    }

    TYPED_TEST(ClosedIntervalTypedTest, Intersection)
    {
        using T = TypeParam;
        const ClosedInterval<T> i(T{10}, T{20});

        // Overlap start
        auto res1 = i.Intersection(ClosedInterval<T>(T{5}, T{15}));
        EXPECT_TRUE(res1.has_value());
        EXPECT_EQ(res1.value(), ClosedInterval<T>(T{10}, T{15}));

        // Overlap end
        auto res2 = i.Intersection(ClosedInterval<T>(T{15}, T{25}));
        EXPECT_TRUE(res2.has_value());
        EXPECT_EQ(res2.value(), ClosedInterval<T>(T{15}, T{20}));

        // Subset
        auto res3 = i.Intersection(ClosedInterval<T>(T{12}, T{18}));
        EXPECT_TRUE(res3.has_value());
        EXPECT_EQ(res3.value(), ClosedInterval<T>(T{12}, T{18}));

        // Superset
        auto res4 = i.Intersection(ClosedInterval<T>(T{5}, T{25}));
        EXPECT_TRUE(res4.has_value());
        EXPECT_EQ(res4.value(), ClosedInterval<T>(T{10}, T{20}));

        // Adjacent (Intersects at a point)
        auto res5 = i.Intersection(ClosedInterval<T>(T{20}, T{25}));
        EXPECT_TRUE(res5.has_value());
        EXPECT_EQ(res5.value(), ClosedInterval<T>(T{20}, T{20}));

        // Disjoint
        auto res6 = i.Intersection(ClosedInterval<T>(T{0}, T{5}));
        EXPECT_FALSE(res6.has_value());

        // Truly Disjoint
        auto res7 = i.Intersection(ClosedInterval<T>(T{0}, T{4}));
        EXPECT_FALSE(res7.has_value());
    }

    TYPED_TEST(ClosedIntervalTypedTest, Clamp)
    {
        using T = TypeParam;
        // Clamp is synonymous with Intersection for this class.
        const ClosedInterval<T> i(T{10}, T{20});
        const ClosedInterval<T> boundary(T{15}, T{25});

        auto res = i.Clamp(boundary);
        EXPECT_TRUE(res.has_value());
        EXPECT_EQ(res.value(), ClosedInterval<T>(T{15}, T{20}));

        auto res2 = boundary.Clamp(i);
        EXPECT_TRUE(res2.has_value());
        EXPECT_EQ(res2.value(), ClosedInterval<T>(T{15}, T{20}));

        const ClosedInterval<T> disjoint_boundary(T{30}, T{40});
        auto res3 = i.Clamp(disjoint_boundary);
        EXPECT_FALSE(res3.has_value());
    }

    TYPED_TEST(ClosedIntervalTypedTest, Merge)
    {
        using T = TypeParam;
        const ClosedInterval<T> i(T{10}, T{20});

        // Overlapping
        auto res1 = i.Merge(ClosedInterval<T>(T{15}, T{25}));
        EXPECT_TRUE(res1.has_value());
        EXPECT_EQ(res1.value(), ClosedInterval<T>(T{10}, T{25}));

        // Adjacent
        auto res2 = i.Merge(ClosedInterval<T>(T{20}, T{25}));
        EXPECT_TRUE(res2.has_value());
        EXPECT_EQ(res2.value(), ClosedInterval<T>(T{10}, T{25}));

        auto res3 = i.Merge(ClosedInterval<T>(T{5}, T{10}));
        EXPECT_TRUE(res3.has_value());
        EXPECT_EQ(res3.value(), ClosedInterval<T>(T{5}, T{20}));

        // Subset
        auto res4 = i.Merge(ClosedInterval<T>(T{12}, T{18}));
        EXPECT_TRUE(res4.has_value());
        EXPECT_EQ(res4.value(), ClosedInterval<T>(T{10}, T{20}));

        // Disjoint
        auto res5 = i.Merge(ClosedInterval<T>(T{21}, T{25}));
        EXPECT_FALSE(res5.has_value());
    }

    TYPED_TEST(ClosedIntervalTypedTest, Combine)
    {
        using T = TypeParam;
        const ClosedInterval<T> i(T{10}, T{20});

        // Overlapping
        auto res1 = i.Combine(ClosedInterval<T>(T{15}, T{25}));
        EXPECT_EQ(res1, ClosedInterval<T>(T{10}, T{25}));

        // Adjacent
        auto res2 = i.Combine(ClosedInterval<T>(T{20}, T{25}));
        EXPECT_EQ(res2, ClosedInterval<T>(T{10}, T{25}));

        // Disjoint (Key difference from Merge)
        auto res3 = i.Combine(ClosedInterval<T>(T{21}, T{25}));
        EXPECT_EQ(res3, ClosedInterval<T>(T{10}, T{25}));

        auto res4 = i.Combine(ClosedInterval<T>(T{0}, T{5}));
        EXPECT_EQ(res4, ClosedInterval<T>(T{0}, T{20}));
    }

    // =========================================================================
    // OpenInterval (start, end)
    // =========================================================================

    template <typename T>
    class OpenIntervalTypedTest : public ::testing::Test
    {
    };

    TYPED_TEST_SUITE(OpenIntervalTypedTest, TestTypes);

    TYPED_TEST(OpenIntervalTypedTest, ConstructionAndGetters)
    {
        using T = TypeParam;
        const T start{0};
        const T end{10};
        const OpenInterval<T> interval(start, end);

        EXPECT_EQ(interval.GetStart(), start);
        EXPECT_EQ(interval.GetEnd(), end);
    }

    TYPED_TEST(OpenIntervalTypedTest, IsEmpty)
    {
        using T = TypeParam;
        // (5, 5) is empty
        const OpenInterval<T> empty_interval(T{5}, T{5});
        const OpenInterval<T> normal_interval(T{5}, T{10});

        EXPECT_TRUE(empty_interval.IsEmpty());
        EXPECT_FALSE(normal_interval.IsEmpty());
    }

    TYPED_TEST(OpenIntervalTypedTest, Length)
    {
        using T = TypeParam;
        const OpenInterval<T> interval(T{5}, T{15});
        const OpenInterval<T> empty_interval(T{5}, T{5});

        EXPECT_EQ(interval.Length(), T{10});
        EXPECT_EQ(empty_interval.Length(), T{0});

        if constexpr (std::is_signed_v<T>)
        {
            const OpenInterval<T> negative_interval(T{-10}, T{0});
            EXPECT_EQ(negative_interval.Length(), T{10});
        }
    }

    TYPED_TEST(OpenIntervalTypedTest, Midpoint)
    {
        using T = TypeParam;
        const OpenInterval<T> interval_even(T{0}, T{10});
        const OpenInterval<T> interval_odd(T{0}, T{5});

        if constexpr (std::is_floating_point_v<T>)
        {
            EXPECT_EQ(interval_even.Midpoint(), T{5.0});
            EXPECT_EQ(interval_odd.Midpoint(), T{2.5});
            if constexpr (std::is_signed_v<T>)
            {
                const OpenInterval<T> interval_negative(T{-10}, T{0});
                EXPECT_EQ(interval_negative.Midpoint(), T{-5.0});
            }
        }
        else
        {
            // Integer division behavior
            EXPECT_EQ(interval_even.Midpoint(), T{5});
            EXPECT_EQ(interval_odd.Midpoint(), T{2}); // 0 + (5-0)/2 = 2
            if constexpr (std::is_signed_v<T>)
            {
                const OpenInterval<T> interval_negative(T{-10}, T{0});
                EXPECT_EQ(interval_negative.Midpoint(), T{-5}); // -10 + (0 - (-10))/2 = -5
            }
        }
    }

    TYPED_TEST(OpenIntervalTypedTest, ContainsValue)
    {
        using T = TypeParam;
        const OpenInterval<T> interval(T{0}, T{10}); // (0, 10)

        // Start is exclusive
        EXPECT_FALSE(interval.Contains(T{0}));
        // Middle
        EXPECT_TRUE(interval.Contains(T{5}));

        // End is exclusive
        EXPECT_FALSE(interval.Contains(T{10}));

        // Values just inside/outside the end
        if constexpr (std::is_floating_point_v<T>)
        {
            EXPECT_TRUE(interval.Contains(T{9.9999}));
            EXPECT_TRUE(interval.Contains(T{0.0001}));
        }
        else
        {
            EXPECT_TRUE(interval.Contains(T{9}));
            EXPECT_TRUE(interval.Contains(T{1}));
        }

        // Outside
        if constexpr (std::is_signed_v<T>)
        {
            EXPECT_FALSE(interval.Contains(T{-1}));
        }
        EXPECT_FALSE(interval.Contains(T{11}));
    }

    TYPED_TEST(OpenIntervalTypedTest, ContainsInterval)
    {
        using T = TypeParam;
        const OpenInterval<T> i(T{0}, T{10});

        EXPECT_TRUE(i.ContainsInterval(OpenInterval<T>(T{2}, T{8}))); // Fully inside
        EXPECT_TRUE(i.ContainsInterval(OpenInterval<T>(T{0}, T{10}))); // Identical
        EXPECT_TRUE(i.ContainsInterval(OpenInterval<T>(T{0}, T{5}))); // Touching start
        EXPECT_TRUE(i.ContainsInterval(OpenInterval<T>(T{5}, T{10}))); // Touching end
        EXPECT_TRUE(i.ContainsInterval(OpenInterval<T>(T{5}, T{5}))); // Empty inside

        if constexpr (std::is_signed_v<T>)
        {
            EXPECT_FALSE(i.ContainsInterval(OpenInterval<T>(T{-1}, T{5}))); // Overlap start
            EXPECT_FALSE(i.ContainsInterval(OpenInterval<T>(T{-1}, T{11}))); // Superset
        }
        EXPECT_FALSE(i.ContainsInterval(OpenInterval<T>(T{5}, T{11}))); // Overlap end
    }

    TYPED_TEST(OpenIntervalTypedTest, Intersects)
    {
        using T = TypeParam;
        const OpenInterval<T> i(T{5}, T{15}); // (5, 15)

        EXPECT_TRUE(i.Intersects(OpenInterval<T>(T{0}, T{10}))); // Overlap start (5, 10)
        EXPECT_TRUE(i.Intersects(OpenInterval<T>(T{10}, T{20}))); // Overlap end (10, 15)
        EXPECT_TRUE(i.Intersects(OpenInterval<T>(T{7}, T{12}))); // Subset
        EXPECT_TRUE(i.Intersects(OpenInterval<T>(T{0}, T{20}))); // Superset

        // Adjacent is NOT intersecting for (start, end)
        EXPECT_FALSE(i.Intersects(OpenInterval<T>(T{0}, T{5}))); // Adjacent start, intersection is (5, 5) (empty)
        EXPECT_FALSE(i.Intersects(OpenInterval<T>(T{15}, T{20}))); // Adjacent end, intersection is (15, 15) (empty)

        // Disjoint
        EXPECT_FALSE(i.Intersects(OpenInterval<T>(T{0}, T{4})));
        EXPECT_FALSE(i.Intersects(OpenInterval<T>(T{16}, T{20})));
    }

    TYPED_TEST(OpenIntervalTypedTest, Adjacent)
    {
        using T = TypeParam;
        const OpenInterval<T> i(T{5}, T{15});

        // Adjacent (touching)
        EXPECT_TRUE(i.Adjacent(OpenInterval<T>(T{0}, T{5})));
        EXPECT_TRUE(i.Adjacent(OpenInterval<T>(T{15}, T{20})));

        // Overlapping is NOT adjacent
        EXPECT_FALSE(i.Adjacent(OpenInterval<T>(T{0}, T{6})));
        EXPECT_FALSE(i.Adjacent(OpenInterval<T>(T{14}, T{20})));
        EXPECT_FALSE(i.Adjacent(OpenInterval<T>(T{7}, T{12})));

        // Disjoint is NOT adjacent
        EXPECT_FALSE(i.Adjacent(OpenInterval<T>(T{0}, T{4})));
        EXPECT_FALSE(i.Adjacent(OpenInterval<T>(T{16}, T{20})));
    }

    TYPED_TEST(OpenIntervalTypedTest, IntersectsOrAdjacent)
    {
        using T = TypeParam;
        const OpenInterval<T> i(T{5}, T{15});

        // Overlapping
        EXPECT_TRUE(i.IntersectsOrAdjacent(OpenInterval<T>(T{0}, T{10})));
        EXPECT_TRUE(i.IntersectsOrAdjacent(OpenInterval<T>(T{10}, T{20})));

        // Adjacent
        EXPECT_TRUE(i.IntersectsOrAdjacent(OpenInterval<T>(T{0}, T{5})));
        EXPECT_TRUE(i.IntersectsOrAdjacent(OpenInterval<T>(T{15}, T{20})));

        // Disjoint
        EXPECT_FALSE(i.IntersectsOrAdjacent(OpenInterval<T>(T{0}, T{4})));
        EXPECT_FALSE(i.IntersectsOrAdjacent(OpenInterval<T>(T{16}, T{20})));
    }

    TYPED_TEST(OpenIntervalTypedTest, DistanceTo)
    {
        using T = TypeParam;
        const OpenInterval<T> i(T{10}, T{20});

        // Overlapping
        EXPECT_EQ(i.DistanceTo(OpenInterval<T>(T{15}, T{25})), T{0});
        // Adjacent
        EXPECT_EQ(i.DistanceTo(OpenInterval<T>(T{20}, T{25})), T{0});
        EXPECT_EQ(i.DistanceTo(OpenInterval<T>(T{5}, T{10})), T{0});

        // Disjoint
        EXPECT_EQ(i.DistanceTo(OpenInterval<T>(T{22}, T{25})), T{2});
        EXPECT_EQ(i.DistanceTo(OpenInterval<T>(T{5}, T{8})), T{2});
    }

    TYPED_TEST(OpenIntervalTypedTest, Intersection)
    {
        using T = TypeParam;
        const OpenInterval<T> i(T{10}, T{20});

        // Overlap start
        auto res1 = i.Intersection(OpenInterval<T>(T{5}, T{15}));
        EXPECT_TRUE(res1.has_value());
        EXPECT_EQ(res1.value(), OpenInterval<T>(T{10}, T{15}));

        // Overlap end
        auto res2 = i.Intersection(OpenInterval<T>(T{15}, T{25}));
        EXPECT_TRUE(res2.has_value());
        EXPECT_EQ(res2.value(), OpenInterval<T>(T{15}, T{20}));

        // Subset
        auto res3 = i.Intersection(OpenInterval<T>(T{12}, T{18}));
        EXPECT_TRUE(res3.has_value());
        EXPECT_EQ(res3.value(), OpenInterval<T>(T{12}, T{18}));

        // Superset
        auto res4 = i.Intersection(OpenInterval<T>(T{5}, T{25}));
        EXPECT_TRUE(res4.has_value());
        EXPECT_EQ(res4.value(), OpenInterval<T>(T{10}, T{20}));

        // Adjacent
        auto res5 = i.Intersection(OpenInterval<T>(T{20}, T{25}));
        EXPECT_FALSE(res5.has_value());

        // Disjoint
        auto res6 = i.Intersection(OpenInterval<T>(T{0}, T{5}));
        EXPECT_FALSE(res6.has_value());
    }

    TYPED_TEST(OpenIntervalTypedTest, Clamp)
    {
        using T = TypeParam;
        // Clamp is synonymous with Intersection for this class.
        const OpenInterval<T> i(T{10}, T{20});
        const OpenInterval<T> boundary(T{15}, T{25});

        auto res = i.Clamp(boundary);
        EXPECT_TRUE(res.has_value());
        EXPECT_EQ(res.value(), OpenInterval<T>(T{15}, T{20}));

        auto res2 = boundary.Clamp(i);
        EXPECT_TRUE(res2.has_value());
        EXPECT_EQ(res2.value(), OpenInterval<T>(T{15}, T{20}));

        const OpenInterval<T> disjoint_boundary(T{30}, T{40});
        auto res3 = i.Clamp(disjoint_boundary);
        EXPECT_FALSE(res3.has_value());
    }

    TYPED_TEST(OpenIntervalTypedTest, Merge)
    {
        using T = TypeParam;
        const OpenInterval<T> i(T{10}, T{20});

        // Overlapping
        auto res1 = i.Merge(OpenInterval<T>(T{15}, T{25}));
        EXPECT_TRUE(res1.has_value());
        EXPECT_EQ(res1.value(), OpenInterval<T>(T{10}, T{25}));

        // Adjacent
        auto res2 = i.Merge(OpenInterval<T>(T{20}, T{25}));
        EXPECT_TRUE(res2.has_value());
        EXPECT_EQ(res2.value(), OpenInterval<T>(T{10}, T{25}));

        auto res3 = i.Merge(OpenInterval<T>(T{5}, T{10}));
        EXPECT_TRUE(res3.has_value());
        EXPECT_EQ(res3.value(), OpenInterval<T>(T{5}, T{20}));

        // Subset
        auto res4 = i.Merge(OpenInterval<T>(T{12}, T{18}));
        EXPECT_TRUE(res4.has_value());
        EXPECT_EQ(res4.value(), OpenInterval<T>(T{10}, T{20}));

        // Disjoint
        auto res5 = i.Merge(OpenInterval<T>(T{21}, T{25}));
        EXPECT_FALSE(res5.has_value());
    }

    TYPED_TEST(OpenIntervalTypedTest, Combine)
    {
        using T = TypeParam;
        const OpenInterval<T> i(T{10}, T{20});

        // Overlapping
        auto res1 = i.Combine(OpenInterval<T>(T{15}, T{25}));
        EXPECT_EQ(res1, OpenInterval<T>(T{10}, T{25}));

        // Adjacent
        auto res2 = i.Combine(OpenInterval<T>(T{20}, T{25}));
        EXPECT_EQ(res2, OpenInterval<T>(T{10}, T{25}));

        // Disjoint (Key difference from Merge)
        auto res3 = i.Combine(OpenInterval<T>(T{21}, T{25}));
        EXPECT_EQ(res3, OpenInterval<T>(T{10}, T{25}));

        auto res4 = i.Combine(OpenInterval<T>(T{0}, T{5}));
        EXPECT_EQ(res4, OpenInterval<T>(T{0}, T{20}));
    }

    class IntervalTest : public ::testing::Test
    {
    };

    TEST_F(IntervalTest, MidpointIntToDouble)
    {
        constexpr ClosedOpenInterval<int> interval_odd(0, 5);
        // Test templated return type for Midpoint
        EXPECT_DOUBLE_EQ(interval_odd.Midpoint<double>(), 2.5);

        constexpr ClosedInterval<int> interval_closed_odd(0, 5);
        EXPECT_DOUBLE_EQ(interval_closed_odd.Midpoint<double>(), 2.5);
    }

    TEST_F(IntervalTest, ContainsDifferentTypes)
    {
        constexpr ClosedOpenInterval<int> i_int(0, 10);
        EXPECT_TRUE(i_int.Contains(5.5));
        EXPECT_FALSE(i_int.Contains(10.1));
        EXPECT_FALSE(i_int.Contains(-0.1));

        constexpr ClosedOpenInterval<double> i_double(0.0, 10.0);
        EXPECT_TRUE(i_double.Contains(5));
        EXPECT_FALSE(i_double.Contains(10));
    }

    TEST_F(IntervalTest, CrossTypeOperations)
    {
        constexpr ClosedOpenInterval<int> i_int(0, 10);
        constexpr ClosedOpenInterval<double> i_double(5.5, 15.5);

        constexpr auto res_int = i_int.Intersection(i_double);
        ASSERT_TRUE(res_int.has_value());
        EXPECT_EQ(res_int.value(), ClosedOpenInterval<int>(5, 10));

        // Test explicit templated return type <double>
        constexpr auto res_double = i_int.Intersection<double, double>(i_double);
        ASSERT_TRUE(res_double.has_value());
        EXPECT_DOUBLE_EQ(res_double.value().GetStart(), 5.5);
        EXPECT_DOUBLE_EQ(res_double.value().GetEnd(), 10.0);

        // Test Merge with explicit return type
        constexpr auto merge_double = i_int.Merge<double, double>(i_double);
        ASSERT_TRUE(merge_double.has_value());
        EXPECT_DOUBLE_EQ(merge_double.value().GetStart(), 0.0);
        EXPECT_DOUBLE_EQ(merge_double.value().GetEnd(), 15.5);

        constexpr ClosedOpenInterval<double> i_int_disjoint(20, 30);
        constexpr auto combine_double = i_double.Combine<double>(i_int_disjoint);
        EXPECT_DOUBLE_EQ(combine_double.GetStart(), 5.5);
        EXPECT_DOUBLE_EQ(combine_double.GetEnd(), 30.0);
    }

    TEST_F(IntervalTest, AbslHash)
    {
        // ClosedOpenInterval
        {
            using IntervalT = ClosedOpenInterval<int>;
            constexpr absl::Hash<IntervalT> hasher;
            constexpr IntervalT i1(0, 10);
            constexpr IntervalT i2(0, 10);
            constexpr IntervalT i3(0, 11);
            EXPECT_EQ(hasher(i1), hasher(i2));
            EXPECT_NE(hasher(i1), hasher(i3));
        }
        // OpenClosedInterval
        {
            using IntervalT = OpenClosedInterval<int>;
            constexpr absl::Hash<IntervalT> hasher;
            constexpr IntervalT i1(0, 10);
            constexpr IntervalT i2(0, 10);
            constexpr IntervalT i3(0, 11);
            EXPECT_EQ(hasher(i1), hasher(i2));
            EXPECT_NE(hasher(i1), hasher(i3));
        }
        // ClosedInterval
        {
            using IntervalT = ClosedInterval<int>;
            constexpr absl::Hash<IntervalT> hasher;
            constexpr IntervalT i1(0, 10);
            constexpr IntervalT i2(0, 10);
            constexpr IntervalT i3(0, 11);
            EXPECT_EQ(hasher(i1), hasher(i2));
            EXPECT_NE(hasher(i1), hasher(i3));
        }
        // OpenInterval
        {
            using IntervalT = OpenInterval<int>;
            constexpr absl::Hash<IntervalT> hasher;
            constexpr IntervalT i1(0, 10);
            constexpr IntervalT i2(0, 10);
            constexpr IntervalT i3(0, 11);
            EXPECT_EQ(hasher(i1), hasher(i2));
            EXPECT_NE(hasher(i1), hasher(i3));
        }
    }

    TEST_F(IntervalTest, AbslStringify)
    {
        constexpr ClosedOpenInterval<int> i_co(-5, 5);
        EXPECT_EQ(absl::StrFormat("%v", i_co), "[-5, 5)");

        constexpr OpenClosedInterval<int> i_oc(-5, 5);
        EXPECT_EQ(absl::StrFormat("%v", i_oc), "(-5, 5]");

        constexpr ClosedInterval<int> i_c(-5, 5);
        EXPECT_EQ(absl::StrFormat("%v", i_c), "[-5, 5]");

        constexpr OpenInterval<int> i_o(-5, 5);
        EXPECT_EQ(absl::StrFormat("%v", i_o), "(-5, 5)");

        constexpr ClosedOpenInterval<double> i_double(0.5, 2.5);
        EXPECT_EQ(absl::StrFormat("%v", i_double), "[0.5, 2.5)");

        constexpr ClosedOpenInterval<int> i_empty(0, 0);
        EXPECT_EQ(absl::StrFormat("%v", i_empty), "[0, 0)");
    }

    using EpsilonTestTypes = ::testing::Types<float, double>;

    template <typename T>
    class EpsilonTypedTest : public ::testing::Test
    {
    };

    TYPED_TEST_SUITE(EpsilonTypedTest, EpsilonTestTypes);

    TYPED_TEST(EpsilonTypedTest, IsEmptyWithEpsilon)
    {
        using T = TypeParam;
        const T epsilon = T{0.1};

        // Length is 0.05, which is <= 0.1, so it should be considered empty
        const ClosedOpenInterval<T> nearly_empty(T{0}, T{0.05});
        EXPECT_TRUE(nearly_empty.IsEmpty(epsilon));

        // Length is 0.15, which is > 0.1, so NOT empty
        const ClosedOpenInterval<T> not_empty(T{0}, T{0.15});
        EXPECT_FALSE(not_empty.IsEmpty(epsilon));

#if BASALT_BUILD_RELEASE
        // ClosedInterval construction forces start <= end.
        // ClosedInterval(0, -0.2) becomes [-0.2, 0].
        // Length is 0.2, which is > 0.1.
        // The class does not support "inverted" intervals via construction.
        const ClosedInterval<T> closed_interval(T{0}, T{-0.2});
        EXPECT_FALSE(closed_interval.IsEmpty(epsilon));
#endif
    }

    TYPED_TEST(EpsilonTypedTest, ContainsValueWithEpsilon)
    {
        using T = TypeParam;
        const T epsilon = T{0.1};
        const ClosedOpenInterval<T> i(T{10}, T{20}); // [10, 20)

        // Strictly outside, but within epsilon
        EXPECT_TRUE(i.Contains(T{9.95}, epsilon)); // 10 - 0.1 = 9.9
        EXPECT_TRUE(i.Contains(T{20.05}, epsilon)); // 20 + 0.1 = 20.1

        // Outside epsilon
        EXPECT_FALSE(i.Contains(T{9.8}, epsilon));
        EXPECT_FALSE(i.Contains(T{20.2}, epsilon));
    }

    TYPED_TEST(EpsilonTypedTest, ContainsIntervalWithEpsilon)
    {
        using T = TypeParam;
        const T epsilon = T{0.1};
        const ClosedOpenInterval<T> i(T{10}, T{20});

        // Slightly larger interval that wouldn't fit strictly, but fits with epsilon
        // i is [10, 20). expanded: [9.9, 20.1)
        // other is [9.95, 20.05)
        const ClosedOpenInterval<T> slightly_larger(T{9.95}, T{20.05});
        EXPECT_TRUE(i.ContainsInterval(slightly_larger, epsilon));

        // Way too big
        const ClosedOpenInterval<T> way_too_big(T{9.0}, T{21.0});
        EXPECT_FALSE(i.ContainsInterval(way_too_big, epsilon));
    }

    TYPED_TEST(EpsilonTypedTest, IntersectsWithEpsilon)
    {
        using T = TypeParam;
        const T epsilon = T{0.1};
        const ClosedOpenInterval<T> i(T{10}, T{20});

        // Strictly disjoint: [20.05, 30)
        // With epsilon 0.1, i effectively reaches 20.1 (conceptually for intersection check)
        // Logic: max(10, 20.05) < min(20, 30) + 0.1
        //        20.05 < 20 + 0.1 -> 20.05 < 20.1 -> TRUE
        EXPECT_TRUE(i.Intersects(ClosedOpenInterval<T>(T{20.05}, T{30}), epsilon));

        // Too far away
        EXPECT_FALSE(i.Intersects(ClosedOpenInterval<T>(T{20.2}, T{30}), epsilon));
    }

    TYPED_TEST(EpsilonTypedTest, AdjacentWithEpsilon)
    {
        using T = TypeParam;
        const T epsilon = T{0.1};
        const ClosedOpenInterval<T> i(T{10}, T{20});

        // Gap is 0.05. Strict Adjacent is false. Epsilon Adjacent is true.
        // [20.05, 30)
        // diff1 = 20.05 - 20 = 0.05. 0.05 <= 0.1 -> True.
        EXPECT_TRUE(i.Adjacent(ClosedOpenInterval<T>(T{20.05}, T{30}), epsilon));

        // Gap is 0.2. False.
        EXPECT_FALSE(i.Adjacent(ClosedOpenInterval<T>(T{20.2}, T{30}), epsilon));
    }

    TYPED_TEST(EpsilonTypedTest, IntersectsOrAdjacentWithEpsilon)
    {
        using T = TypeParam;
        const T epsilon = T{0.1};
        const ClosedOpenInterval<T> i(T{10}, T{20});

        // Gap of 0.05
        EXPECT_TRUE(i.IntersectsOrAdjacent(ClosedOpenInterval<T>(T{20.05}, T{30}), epsilon));

        // Gap of 0.2
        EXPECT_FALSE(i.IntersectsOrAdjacent(ClosedOpenInterval<T>(T{20.2}, T{30}), epsilon));
    }

    TYPED_TEST(EpsilonTypedTest, MergeWithEpsilon)
    {
        using T = TypeParam;
        const T epsilon = T{0.5};
        const ClosedOpenInterval<T> i1(T{0}, T{10});
        const ClosedOpenInterval<T> i2(T{10.4}, T{20}); // Gap of 0.4

        // Strictly, these don't touch or intersect.
        EXPECT_FALSE(i1.Merge(i2).has_value());

        // With epsilon 0.5, they should merge.
        auto res = i1.Merge(i2, epsilon);
        ASSERT_TRUE(res.has_value());
        // Result should span min start to max end: [0, 20)
        // Note: The gap is bridged, consuming the empty space.
        EXPECT_EQ(res.value(), ClosedOpenInterval<T>(T{0}, T{20}));

        const ClosedOpenInterval<T> i3(T{10.6}, T{20}); // Gap of 0.6
        EXPECT_FALSE(i1.Merge(i3, epsilon).has_value());
    }

    // Explicit tests for different interval types to ensure template coverage
    TEST_F(IntervalTest, EpsilonOverloadsDifferentTypes)
    {
        constexpr double eps = 0.1;

        // OpenClosed (10, 20]
        const OpenClosedInterval<double> oc(10.0, 20.0);
        // Check near start (9.95 is outside strict, inside with eps)
        EXPECT_TRUE(oc.Contains(9.95, eps));
        // Check near end (20.05 is outside strict, inside with eps)
        EXPECT_TRUE(oc.Contains(20.05, eps));

        // Closed [10, 20]
        const ClosedInterval<double> c(10.0, 20.0);
        // Adjacent with gap
        const ClosedInterval<double> c_gap(20.05, 30.0);
        EXPECT_TRUE(c.Adjacent(c_gap, eps));

        // Open (10, 20)
        const OpenInterval<double> o(10.0, 20.0);
        // Intersection with gap
        const OpenInterval<double> o_gap(20.05, 30.0);
        EXPECT_TRUE(o.Intersects(o_gap, eps));
    }

    TEST_F(IntervalTest, StdHash)
    {
        // ClosedOpenInterval
        {
            using IntervalT = ClosedOpenInterval<int>;
            std::hash<IntervalT> hasher;
            IntervalT i1(0, 10);
            IntervalT i2(0, 10);
            IntervalT i3(0, 11);
            EXPECT_EQ(hasher(i1), hasher(i2));
            EXPECT_NE(hasher(i1), hasher(i3));
        }
        // OpenClosedInterval
        {
            using IntervalT = OpenClosedInterval<int>;
            std::hash<IntervalT> hasher;
            IntervalT i1(0, 10);
            IntervalT i2(0, 10);
            IntervalT i3(0, 11);
            EXPECT_EQ(hasher(i1), hasher(i2));
            EXPECT_NE(hasher(i1), hasher(i3));
        }
        // ClosedInterval
        {
            using IntervalT = ClosedInterval<int>;
            std::hash<IntervalT> hasher;
            IntervalT i1(0, 10);
            IntervalT i2(0, 10);
            IntervalT i3(0, 11);
            EXPECT_EQ(hasher(i1), hasher(i2));
            EXPECT_NE(hasher(i1), hasher(i3));
        }
        // OpenInterval
        {
            using IntervalT = OpenInterval<int>;
            std::hash<IntervalT> hasher;
            IntervalT i1(0, 10);
            IntervalT i2(0, 10);
            IntervalT i3(0, 11);
            EXPECT_EQ(hasher(i1), hasher(i2));
            EXPECT_NE(hasher(i1), hasher(i3));
        }
    }
} // namespace bslt::test
