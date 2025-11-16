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

#include "basalt/math/interval.h"
#include "gtest/gtest.h"
#include "absl/hash/hash.h"
#include "absl/strings/str_format.h"

namespace bslt::test
{
    // Static assert to ensure the concept is satisfied.
    static_assert(Interval<ClosedOpenInterval<int32_t>>, "ClosedOpenInterval<int32_t> should satisfy the Interval concept");
    static_assert(Interval<ClosedOpenInterval<double>>, "ClosedOpenInterval<double> should satisfy the Interval concept");

    // Typed test suite for basic interval logic across different arithmetic types
    template <typename T>
    class IntervalTypedTest : public ::testing::Test {};

    using TestTypes = ::testing::Types<int, float, double, size_t>;
    TYPED_TEST_SUITE(IntervalTypedTest, TestTypes);

    TYPED_TEST(IntervalTypedTest, ConstructionAndGetters)
    {
        using T = TypeParam;
        const T start{0};
        const T end{10};
        const ClosedOpenInterval<T> interval(start, end);

        EXPECT_EQ(interval.GetStart(), start);
        EXPECT_EQ(interval.GetEnd(), end);
    }

    TYPED_TEST(IntervalTypedTest, ConstructionSwapsOrder)
    {
        using T = TypeParam;
        const T start{0};
        const T end{10};
        const ClosedOpenInterval<T> interval(end, start); // Swapped

        EXPECT_EQ(interval.GetStart(), start);
        EXPECT_EQ(interval.GetEnd(), end);
    }

    TYPED_TEST(IntervalTypedTest, IsEmpty)
    {
        using T = TypeParam;
        const ClosedOpenInterval<T> empty_interval(T{5}, T{5});
        const ClosedOpenInterval<T> normal_interval(T{5}, T{10});

        EXPECT_TRUE(empty_interval.IsEmpty());
        EXPECT_FALSE(normal_interval.IsEmpty());
    }

    TYPED_TEST(IntervalTypedTest, Length)
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

    TYPED_TEST(IntervalTypedTest, Midpoint)
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

    TYPED_TEST(IntervalTypedTest, ContainsValue)
    {
        using T = TypeParam;
        const ClosedOpenInterval<T> interval(T{0}, T{10});

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

    TYPED_TEST(IntervalTypedTest, ContainsInterval)
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

    TYPED_TEST(IntervalTypedTest, Intersects)
    {
        using T = TypeParam;
        const ClosedOpenInterval<T> i(T{5}, T{15});

        EXPECT_TRUE(i.Intersects(ClosedOpenInterval<T>(T{0}, T{10})));  // Overlap start
        EXPECT_TRUE(i.Intersects(ClosedOpenInterval<T>(T{10}, T{20}))); // Overlap end
        EXPECT_TRUE(i.Intersects(ClosedOpenInterval<T>(T{7}, T{12})));  // Subset
        EXPECT_TRUE(i.Intersects(ClosedOpenInterval<T>(T{0}, T{20})));  // Superset

        // Adjacent is NOT intersecting
        EXPECT_FALSE(i.Intersects(ClosedOpenInterval<T>(T{0}, T{5})));  // Adjacent start
        EXPECT_FALSE(i.Intersects(ClosedOpenInterval<T>(T{15}, T{20}))); // Adjacent end

        // Disjoint
        EXPECT_FALSE(i.Intersects(ClosedOpenInterval<T>(T{0}, T{4})));
        EXPECT_FALSE(i.Intersects(ClosedOpenInterval<T>(T{16}, T{20})));
    }

    TYPED_TEST(IntervalTypedTest, Adjacent)
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

    TYPED_TEST(IntervalTypedTest, IntersectsOrAdjacent)
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

    TYPED_TEST(IntervalTypedTest, DistanceTo)
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

    TYPED_TEST(IntervalTypedTest, Intersection)
    {
        using T = TypeParam;
        const ClosedOpenInterval<T> i(T{10}, T{20});

        // Overlap start
        auto res1 = i.Intersection(ClosedOpenInterval<T>(T{5}, T{15}));
        EXPECT_TRUE(res1.has_value());
        // EXPECT_EQ now works because operator== is in namespace bslt
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

    TYPED_TEST(IntervalTypedTest, Clamp)
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

    TYPED_TEST(IntervalTypedTest, Merge)
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

    TYPED_TEST(IntervalTypedTest, Combine)
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

    class IntervalTest : public ::testing::Test {};

    TEST_F(IntervalTest, MidpointIntToDouble)
    {
        constexpr ClosedOpenInterval<int> interval_odd(0, 5);
        // Test templated return type for Midpoint
        EXPECT_DOUBLE_EQ(interval_odd.Midpoint<double>(), 2.5);
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
        using IntervalInt = ClosedOpenInterval<int>;
        constexpr absl::Hash<IntervalInt> hasher;

        constexpr IntervalInt i1(0, 10);
        constexpr IntervalInt i2(0, 10);
        constexpr IntervalInt i3(0, 11);
        constexpr IntervalInt i4(1, 10);

        EXPECT_EQ(hasher(i1), hasher(i2));
        EXPECT_NE(hasher(i1), hasher(i3));
        EXPECT_NE(hasher(i1), hasher(i4));
    }

    TEST_F(IntervalTest, AbslStringify)
    {
        constexpr ClosedOpenInterval<int> i_int(-5, 5);
        EXPECT_EQ(absl::StrFormat("%v", i_int), "[-5, 5)");

        constexpr ClosedOpenInterval<double> i_double(0.5, 2.5);
        EXPECT_EQ(absl::StrFormat("%v", i_double), "[0.5, 2.5)");

        constexpr ClosedOpenInterval<int> i_empty(0, 0);
        EXPECT_EQ(absl::StrFormat("%v", i_empty), "[0, 0)");
    }
} // namespace bslt::test
