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

#include "basalt/math/interval.h"
#include "gtest/gtest.h"
#include <limits>
#include <optional>

namespace bslt::test
{
    TEST(ClosedOpenIntervalTest, ConstructorAndGetters)
    {
        constexpr ClosedOpenInterval<int> i(10, 20);
        EXPECT_EQ(i.GetStart(), 10);
        EXPECT_EQ(i.GetEnd(), 20);

        // Test auto-sorting
        constexpr ClosedOpenInterval<int> i_reversed(20, 10);
        EXPECT_EQ(i_reversed.GetStart(), 10);
        EXPECT_EQ(i_reversed.GetEnd(), 20);

        constexpr ClosedOpenInterval<int> i_empty(10, 10);
        EXPECT_EQ(i_empty.GetStart(), 10);
        EXPECT_EQ(i_empty.GetEnd(), 10);

        constexpr ClosedOpenInterval<double> d(10.5, 20.5);
        EXPECT_EQ(d.GetStart(), 10.5);
        EXPECT_EQ(d.GetEnd(), 20.5);
    }

    TEST(ClosedOpenIntervalTest, Midpoint)
    {
        constexpr ClosedOpenInterval<int> i_even(10, 20);
        EXPECT_EQ(i_even.Midpoint(), 15); // 10 + (20-10)/2 = 15

        constexpr ClosedOpenInterval<int> i_odd(10, 21);
        EXPECT_EQ(i_odd.Midpoint(), 15); // 10 + (21-10)/2 = 10 + 5 = 15

        constexpr ClosedOpenInterval<double> d(10.0, 21.0);
        EXPECT_EQ(d.Midpoint(), 15.5); // (10.0 + 21.0) / 2 = 15.5

        constexpr ClosedOpenInterval<int> i_neg(-20, -10);
        EXPECT_EQ(i_neg.Midpoint(), -15); // -20 + (-10 - -20)/2 = -20 + 5 = -15

        // Test template return type
        EXPECT_EQ(i_even.Midpoint<double>(), 15.0);
        EXPECT_EQ(d.Midpoint<int>(), 15);
    }

    TEST(ClosedOpenIntervalTest, IsEmptyAndLength)
    {
        constexpr ClosedOpenInterval<int> i(10, 20);
        EXPECT_FALSE(i.IsEmpty());
        EXPECT_EQ(i.Length(), 10);

        constexpr ClosedOpenInterval<int> i_empty(10, 10);
        EXPECT_TRUE(i_empty.IsEmpty());
        EXPECT_EQ(i_empty.Length(), 0);

        constexpr ClosedOpenInterval<double> d(10.5, 11.0);
        EXPECT_FALSE(d.IsEmpty());
        EXPECT_DOUBLE_EQ(d.Length(), 0.5);
    }

    TEST(ClosedOpenIntervalTest, Contains)
    {
        constexpr ClosedOpenInterval<int> i(10, 20); // [10, 20)
        EXPECT_FALSE(i.Contains(9));
        EXPECT_TRUE(i.Contains(10));  // Inclusive start
        EXPECT_TRUE(i.Contains(15));
        EXPECT_TRUE(i.Contains(19));
        EXPECT_FALSE(i.Contains(20)); // Exclusive end
        EXPECT_FALSE(i.Contains(21));

        // Test with different types
        EXPECT_TRUE(i.Contains(10.0));
        EXPECT_TRUE(i.Contains(19.999));
        EXPECT_FALSE(i.Contains(20.0));

        constexpr ClosedOpenInterval<int> i_empty(10, 10);
        EXPECT_FALSE(i_empty.Contains(10));
    }

    TEST(ClosedOpenIntervalTest, ContainsInterval)
    {
        constexpr ClosedOpenInterval<int> i(10, 20); // [10, 20)
        EXPECT_TRUE(i.ContainsInterval(ClosedOpenInterval<int>(10, 20))); // Identical
        EXPECT_TRUE(i.ContainsInterval(ClosedOpenInterval<int>(12, 18))); // Fully inside
        EXPECT_TRUE(i.ContainsInterval(ClosedOpenInterval<int>(10, 18))); // Aligned start
        EXPECT_TRUE(i.ContainsInterval(ClosedOpenInterval<int>(12, 20))); // Aligned end
        EXPECT_TRUE(i.ContainsInterval(ClosedOpenInterval<int>(10, 10))); // Empty at start
        EXPECT_TRUE(i.ContainsInterval(ClosedOpenInterval<int>(15, 15))); // Empty inside

        EXPECT_FALSE(i.ContainsInterval(ClosedOpenInterval<int>(8, 12)));  // Overlap start
        EXPECT_FALSE(i.ContainsInterval(ClosedOpenInterval<int>(18, 22))); // Overlap end
        EXPECT_FALSE(i.ContainsInterval(ClosedOpenInterval<int>(8, 22)));  // Contains this
        EXPECT_FALSE(i.ContainsInterval(ClosedOpenInterval<int>(0, 5)));   // Disjoint
    }

    TEST(ClosedOpenIntervalTest, Intersects)
    {
        constexpr ClosedOpenInterval<int> i(10, 20); // [10, 20)
        EXPECT_TRUE(i.Intersects(ClosedOpenInterval<int>(10, 20))); // Identical
        EXPECT_TRUE(i.Intersects(ClosedOpenInterval<int>(12, 18))); // Fully inside
        EXPECT_TRUE(i.Intersects(ClosedOpenInterval<int>(8, 12)));  // Overlap start
        EXPECT_TRUE(i.Intersects(ClosedOpenInterval<int>(18, 22))); // Overlap end
        EXPECT_TRUE(i.Intersects(ClosedOpenInterval<int>(8, 22)));  // Contains this

        // Disjoint
        EXPECT_FALSE(i.Intersects(ClosedOpenInterval<int>(0, 5)));   // Disjoint before
        EXPECT_FALSE(i.Intersects(ClosedOpenInterval<int>(25, 30))); // Disjoint after

        // Touching boundaries (exclusive end means no intersection)
        EXPECT_FALSE(i.Intersects(ClosedOpenInterval<int>(0, 10)));   // Touches start [0, 10)
        EXPECT_FALSE(i.Intersects(ClosedOpenInterval<int>(20, 30))); // Touches end [20, 30)

        // Empty
        EXPECT_FALSE(i.Intersects(ClosedOpenInterval<int>(10, 10)));
        EXPECT_FALSE(i.Intersects(ClosedOpenInterval<int>(0, 0)));
    }

    TEST(ClosedOpenIntervalTest, Intersection)
    {
        ClosedOpenInterval<int> i(10, 20); // [10, 20)

        // Overlap end
        auto res1 = i.Intersection(ClosedOpenInterval<int>(15, 25)); // [15, 20)
        EXPECT_TRUE(res1.has_value());
        EXPECT_EQ(res1->GetStart(), 15);
        EXPECT_EQ(res1->GetEnd(), 20);

        // Overlap start
        auto res2 = i.Intersection(ClosedOpenInterval<int>(5, 15)); // [10, 15)
        EXPECT_TRUE(res2.has_value());
        EXPECT_EQ(res2->GetStart(), 10);
        EXPECT_EQ(res2->GetEnd(), 15);

        // Fully inside
        auto res3 = i.Intersection(ClosedOpenInterval<int>(12, 18)); // [12, 18)
        EXPECT_TRUE(res3.has_value());
        EXPECT_EQ(res3->GetStart(), 12);
        EXPECT_EQ(res3->GetEnd(), 18);

        // Contains this
        auto res4 = i.Intersection(ClosedOpenInterval<int>(5, 25)); // [10, 20)
        EXPECT_TRUE(res4.has_value());
        EXPECT_EQ(res4->GetStart(), 10);
        EXPECT_EQ(res4->GetEnd(), 20);

        // Disjoint (touching)
        EXPECT_FALSE(i.Intersection(ClosedOpenInterval<int>(0, 10)).has_value());
        EXPECT_FALSE(i.Intersection(ClosedOpenInterval<int>(20, 30)).has_value());

        // Disjoint (far)
        EXPECT_FALSE(i.Intersection(ClosedOpenInterval<int>(0, 5)).has_value());

        // Test template return type
        auto res_double = i.Intersection<int, double>(ClosedOpenInterval<int>(15, 25));
        EXPECT_TRUE(res_double.has_value());
        EXPECT_DOUBLE_EQ(res_double->GetStart(), 15.0);
        EXPECT_DOUBLE_EQ(res_double->GetEnd(), 20.0);
    }

    TEST(ClosedOpenIntervalTest, Clamp)
    {
        ClosedOpenInterval<int> i(10, 20); // [10, 20)
        // Clamp to boundary [15, 25) -> [15, 20)
        auto res1 = i.Clamp(ClosedOpenInterval<int>(15, 25));
        EXPECT_TRUE(res1.has_value());
        EXPECT_EQ(res1.value(), ClosedOpenInterval<int>(15, 20));

        // Clamp to boundary [5, 15) -> [10, 15)
        auto res2 = i.Clamp(ClosedOpenInterval<int>(5, 15));
        EXPECT_TRUE(res2.has_value());
        EXPECT_EQ(res2.value(), ClosedOpenInterval<int>(10, 15));

        // Clamp to boundary [12, 18) (inside) -> [12, 18)
        auto res3 = i.Clamp(ClosedOpenInterval<int>(12, 18));
        EXPECT_TRUE(res3.has_value());
        EXPECT_EQ(res3.value(), ClosedOpenInterval<int>(12, 18));

        // Clamp to boundary [5, 25) (outside) -> [10, 20)
        auto res4 = i.Clamp(ClosedOpenInterval<int>(5, 25));
        EXPECT_TRUE(res4.has_value());
        EXPECT_EQ(res4.value(), ClosedOpenInterval<int>(10, 20));

        // No overlap
        EXPECT_FALSE(i.Clamp(ClosedOpenInterval<int>(0, 10)).has_value());
        EXPECT_FALSE(i.Clamp(ClosedOpenInterval<int>(20, 30)).has_value());
    }

    TEST(ClosedOpenIntervalTest, Equality)
    {
        EXPECT_TRUE(ClosedOpenInterval<int>(10, 20) == ClosedOpenInterval<int>(10, 20));
        EXPECT_FALSE(ClosedOpenInterval<int>(10, 20) == ClosedOpenInterval<int>(10, 21));
        EXPECT_FALSE(ClosedOpenInterval<int>(10, 20) == ClosedOpenInterval<int>(11, 20));
        EXPECT_TRUE(ClosedOpenInterval<int>(10, 20) != ClosedOpenInterval<int>(11, 20));

        // Test different types
        EXPECT_TRUE(ClosedOpenInterval<int>(10, 20) == ClosedOpenInterval<double>(10.0, 20.0));
        EXPECT_FALSE(ClosedOpenInterval<int>(10, 20) == ClosedOpenInterval<double>(10.0, 20.1));
    }

    TEST(ClosedOpenIntervalTest, Stringify)
    {
        constexpr ClosedOpenInterval<int> i(10, 20);
        EXPECT_EQ(absl::StrFormat("%v", i), "[10, 20)");

        constexpr ClosedOpenInterval<double> d(-1.5, 2.5);
        EXPECT_EQ(absl::StrFormat("%v", d), "[-1.5, 2.5)");
    }

    // --- OpenClosedInterval (start, end] ---

    TEST(OpenClosedIntervalTest, ConstructorAndGetters)
    {
        constexpr OpenClosedInterval<int> i(10, 20);
        EXPECT_EQ(i.GetStart(), 10);
        EXPECT_EQ(i.GetEnd(), 20);

        constexpr OpenClosedInterval<int> i_reversed(20, 10);
        EXPECT_EQ(i_reversed.GetStart(), 10);
        EXPECT_EQ(i_reversed.GetEnd(), 20);
    }

    TEST(OpenClosedIntervalTest, IsEmptyAndLength)
    {
        constexpr OpenClosedInterval<int> i(10, 20);
        EXPECT_FALSE(i.IsEmpty());
        EXPECT_EQ(i.Length(), 10);

        constexpr OpenClosedInterval<int> i_empty(10, 10);
        EXPECT_TRUE(i_empty.IsEmpty());
        EXPECT_EQ(i_empty.Length(), 0);
    }

    TEST(OpenClosedIntervalTest, Contains)
    {
        constexpr OpenClosedInterval<int> i(10, 20); // (10, 20]
        EXPECT_FALSE(i.Contains(9));
        EXPECT_FALSE(i.Contains(10)); // Exclusive start
        EXPECT_TRUE(i.Contains(11));
        EXPECT_TRUE(i.Contains(15));
        EXPECT_TRUE(i.Contains(20));  // Inclusive end
        EXPECT_FALSE(i.Contains(21));

        // Test with different types
        EXPECT_FALSE(i.Contains(10.0));
        EXPECT_TRUE(i.Contains(10.0001));
        EXPECT_TRUE(i.Contains(20.0));

        constexpr OpenClosedInterval<int> i_empty(10, 10);
        EXPECT_FALSE(i_empty.Contains(10));
    }

    TEST(OpenClosedIntervalTest, Intersects)
    {
        constexpr OpenClosedInterval<int> i(10, 20); // (10, 20]
        EXPECT_TRUE(i.Intersects(OpenClosedInterval<int>(10, 20))); // Identical
        EXPECT_TRUE(i.Intersects(OpenClosedInterval<int>(8, 12)));  // Overlap start
        EXPECT_TRUE(i.Intersects(OpenClosedInterval<int>(18, 22))); // Overlap end
        EXPECT_TRUE(i.Intersects(OpenClosedInterval<int>(8, 22)));  // Contains this

        // Disjoint
        EXPECT_FALSE(i.Intersects(OpenClosedInterval<int>(0, 5)));

        // Touching boundaries (exclusive start means no intersection)
        EXPECT_FALSE(i.Intersects(OpenClosedInterval<int>(0, 10)));   // Touches start (0, 10]
        EXPECT_FALSE(i.Intersects(OpenClosedInterval<int>(20, 30))); // Touches end (20, 30]
    }

    TEST(OpenClosedIntervalTest, Intersection)
    {
        constexpr OpenClosedInterval<int> i(10, 20); // (10, 20]

        // Overlap end
        const auto res1 = i.Intersection(OpenClosedInterval<int>(15, 25)); // (15, 20]
        EXPECT_TRUE(res1.has_value());
        EXPECT_EQ(res1.value(), OpenClosedInterval<int>(15, 20));

        // Overlap start
        const auto res2 = i.Intersection(OpenClosedInterval<int>(5, 15)); // (10, 15]
        EXPECT_TRUE(res2.has_value());
        EXPECT_EQ(res2.value(), OpenClosedInterval<int>(10, 15));

        // Disjoint (touching)
        EXPECT_FALSE(i.Intersection(OpenClosedInterval<int>(0, 10)).has_value());
        EXPECT_FALSE(i.Intersection(OpenClosedInterval<int>(20, 30)).has_value());
    }

    TEST(OpenClosedIntervalTest, Stringify)
    {
        constexpr OpenClosedInterval<int> i(10, 20);
        EXPECT_EQ(absl::StrFormat("%v", i), "(10, 20]");
    }

    // --- ClosedInterval [start, end] ---

    TEST(ClosedIntervalTest, ConstructorAndGetters)
    {
        constexpr ClosedInterval<int> i(10, 20);
        EXPECT_EQ(i.GetStart(), 10);
        EXPECT_EQ(i.GetEnd(), 20);

        constexpr ClosedInterval<int> i_reversed(20, 10);
        EXPECT_EQ(i_reversed.GetStart(), 10);
        EXPECT_EQ(i_reversed.GetEnd(), 20);
    }

    TEST(ClosedIntervalTest, IsEmptyAndLength)
    {
        constexpr ClosedInterval<int> i(10, 20);
        EXPECT_FALSE(i.IsEmpty());
        EXPECT_EQ(i.Length(), 10);

        // A single-point interval is not empty
        constexpr ClosedInterval<int> i_point(10, 10);
        EXPECT_FALSE(i_point.IsEmpty());
        EXPECT_EQ(i_point.Length(), 0);
    }

    TEST(ClosedIntervalTest, Contains)
    {
        constexpr ClosedInterval<int> i(10, 20); // [10, 20]
        EXPECT_FALSE(i.Contains(9));
        EXPECT_TRUE(i.Contains(10));  // Inclusive start
        EXPECT_TRUE(i.Contains(15));
        EXPECT_TRUE(i.Contains(20));  // Inclusive end
        EXPECT_FALSE(i.Contains(21));

        // Test with different types
        EXPECT_TRUE(i.Contains(10.0));
        EXPECT_TRUE(i.Contains(20.0));

        constexpr ClosedInterval<int> i_point(10, 10);
        EXPECT_TRUE(i_point.Contains(10));
        EXPECT_FALSE(i_point.Contains(11));
    }

    TEST(ClosedIntervalTest, Intersects)
    {
        constexpr ClosedInterval<int> i(10, 20); // [10, 20]
        EXPECT_TRUE(i.Intersects(ClosedInterval<int>(10, 20))); // Identical
        EXPECT_TRUE(i.Intersects(ClosedInterval<int>(8, 12)));  // Overlap start
        EXPECT_TRUE(i.Intersects(ClosedInterval<int>(18, 22))); // Overlap end
        EXPECT_TRUE(i.Intersects(ClosedInterval<int>(8, 22)));  // Contains this

        // Disjoint
        EXPECT_FALSE(i.Intersects(ClosedInterval<int>(0, 5)));
        EXPECT_FALSE(i.Intersects(ClosedInterval<int>(0, 9)));

        // Touching boundaries (inclusive end means intersection)
        EXPECT_TRUE(i.Intersects(ClosedInterval<int>(0, 10)));   // Touches start [0, 10]
        EXPECT_TRUE(i.Intersects(ClosedInterval<int>(20, 30))); // Touches end [20, 30]
    }

    TEST(ClosedIntervalTest, Intersection)
    {
        ClosedInterval<int> i(10, 20); // [10, 20]

        // Overlap end
        auto res1 = i.Intersection(ClosedInterval<int>(15, 25)); // [15, 20]
        EXPECT_TRUE(res1.has_value());
        EXPECT_EQ(res1.value(), ClosedInterval<int>(15, 20));

        // Overlap start
        auto res2 = i.Intersection(ClosedInterval<int>(5, 15)); // [10, 15]
        EXPECT_TRUE(res2.has_value());
        EXPECT_EQ(res2.value(), ClosedInterval<int>(10, 15));

        // Disjoint (touching)
        auto res3 = i.Intersection(ClosedInterval<int>(0, 10)); // [10, 10]
        EXPECT_TRUE(res3.has_value());
        EXPECT_EQ(res3.value(), ClosedInterval<int>(10, 10));

        auto res4 = i.Intersection(ClosedInterval<int>(20, 30)); // [20, 20]
        EXPECT_TRUE(res4.has_value());
        EXPECT_EQ(res4.value(), ClosedInterval<int>(20, 20));

        // Disjoint (far)
        EXPECT_FALSE(i.Intersection(ClosedInterval<int>(0, 9)).has_value());
        EXPECT_FALSE(i.Intersection(ClosedInterval<int>(21, 30)).has_value());
    }

    TEST(ClosedIntervalTest, Stringify)
    {
        constexpr ClosedInterval<int> i(10, 20);
        EXPECT_EQ(absl::StrFormat("%v", i), "[10, 20]");
    }

    // --- OpenInterval (start, end) ---

    TEST(OpenIntervalTest, ConstructorAndGetters)
    {
        constexpr OpenInterval<int> i(10, 20);
        EXPECT_EQ(i.GetStart(), 10);
        EXPECT_EQ(i.GetEnd(), 20);

        constexpr OpenInterval<int> i_reversed(20, 10);
        EXPECT_EQ(i_reversed.GetStart(), 10);
        EXPECT_EQ(i_reversed.GetEnd(), 20);
    }

    TEST(OpenIntervalTest, IsEmptyAndLength)
    {
        constexpr OpenInterval<int> i(10, 20);
        EXPECT_FALSE(i.IsEmpty());
        EXPECT_EQ(i.Length(), 10);

        constexpr OpenInterval<int> i_empty(10, 10);
        EXPECT_TRUE(i_empty.IsEmpty());
        EXPECT_EQ(i_empty.Length(), 0);
    }

    TEST(OpenIntervalTest, Contains)
    {
        constexpr OpenInterval<int> i(10, 20); // (10, 20)
        EXPECT_FALSE(i.Contains(9));
        EXPECT_FALSE(i.Contains(10)); // Exclusive start
        EXPECT_TRUE(i.Contains(11));
        EXPECT_TRUE(i.Contains(15));
        EXPECT_TRUE(i.Contains(19));
        EXPECT_FALSE(i.Contains(20)); // Exclusive end
        EXPECT_FALSE(i.Contains(21));

        // Test with different types
        EXPECT_FALSE(i.Contains(10.0));
        EXPECT_TRUE(i.Contains(10.0001));
        EXPECT_TRUE(i.Contains(19.9999));
        EXPECT_FALSE(i.Contains(20.0));

        constexpr OpenInterval<int> i_empty(10, 10);
        EXPECT_FALSE(i_empty.Contains(10));
    }

    TEST(OpenIntervalTest, Intersects)
    {
        constexpr OpenInterval<int> i(10, 20); // (10, 20)
        EXPECT_TRUE(i.Intersects(OpenInterval<int>(10, 20))); // Identical
        EXPECT_TRUE(i.Intersects(OpenInterval<int>(8, 12)));  // Overlap start
        EXPECT_TRUE(i.Intersects(OpenInterval<int>(18, 22))); // Overlap end
        EXPECT_TRUE(i.Intersects(OpenInterval<int>(8, 22)));  // Contains this

        // Disjoint
        EXPECT_FALSE(i.Intersects(OpenInterval<int>(0, 5)));

        // Touching boundaries (all exclusive means no intersection)
        EXPECT_FALSE(i.Intersects(OpenInterval<int>(0, 10)));
        // (10, 20) vs (0, 11). max(10,0)=10. min(20,11)=11. 10 < 11. Intersects.
        EXPECT_TRUE(i.Intersects(OpenInterval<int>(0, 11)));
        EXPECT_FALSE(i.Intersects(OpenInterval<int>(20, 30)));
    }

    TEST(OpenIntervalTest, Intersection)
    {
        constexpr OpenInterval<int> i(10, 20); // (10, 20)

        // Overlap end
        const auto res1 = i.Intersection(OpenInterval<int>(15, 25)); // (15, 20)
        EXPECT_TRUE(res1.has_value());
        EXPECT_EQ(res1.value(), OpenInterval<int>(15, 20));

        // Overlap start
        const auto res2 = i.Intersection(OpenInterval<int>(5, 15)); // (10, 15)
        EXPECT_TRUE(res2.has_value());
        EXPECT_EQ(res2.value(), OpenInterval<int>(10, 15));

        // Disjoint (touching)
        EXPECT_FALSE(i.Intersection(OpenInterval<int>(0, 10)).has_value());
        EXPECT_FALSE(i.Intersection(OpenInterval<int>(20, 30)).has_value());

        // Disjoint (far)
        EXPECT_FALSE(i.Intersection(OpenInterval<int>(0, 5)).has_value());
    }

    TEST(OpenIntervalTest, Stringify)
    {
        constexpr OpenInterval<int> i(10, 20);
        EXPECT_EQ(absl::StrFormat("%v", i), "(10, 20)");
    }

} // namespace bslt::test
