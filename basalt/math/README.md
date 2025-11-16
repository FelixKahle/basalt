# Math

This module provides mathematical utilities and data structures for the Basalt library.

## Overview

The math module contains fundamental mathematical functions and geometric data structures commonly used in scientific computing applications. It provides efficient, type-safe implementations with support for compile-time evaluation where possible.

## Components

### Math Functions ([math.h](basalt/math/math.h))

Core mathematical utilities that extend the standard library with type-safe, generic implementations:

- **[`bslt::Abs`](basalt/math/math.h)** - Unified absolute value function for all arithmetic types
- **[`bslt::AlmostEqual`](basalt/math/math.h)** - Robust floating-point and integer equality comparison with configurable tolerance
- **[`bslt::IsPowerOfTwo`](basalt/math/math.h)** - Efficient power-of-two detection for integral types

### Interval Types ([interval.h](basalt/math/interval.h))

Mathematical interval representations with different boundary semantics:

- **[`bslt::ClosedOpenInterval<T>`](basalt/math/interval.h)** - `[start, end)` intervals (inclusive start, exclusive end)
- **[`bslt::OpenClosedInterval<T>`](basalt/math/interval.h)** - `(start, end]` intervals (exclusive start, inclusive end)  
- **[`bslt::ClosedInterval<T>`](basalt/math/interval.h)** - `[start, end]` intervals (inclusive both ends)
- **[`bslt::OpenInterval<T>`](basalt/math/interval.h)** - `(start, end)` intervals (exclusive both ends)

## Features

### Type Safety
- Template-based implementations with concept constraints
- Automatic type promotion and conversion handling
- Compile-time type checking

### Performance
- `constexpr` and `BASALT_FORCE_INLINE` for zero-overhead abstractions
- Optimized algorithms for common operations
- Branch-free implementations where applicable

### Interval Operations
All interval types support:
- **Containment testing** - Check if values or other intervals are contained
- **Intersection** - Compute overlapping regions between intervals
- **Clamping** - Restrict intervals to boundary constraints
- **Geometric queries** - Length, midpoint, emptiness checks
- **Formatting** - Absl string formatting support with mathematical notation
- **Hashing** - Absl hash integration for use in containers

## Usage Examples

```cpp
#include "basalt/math/math.h"
#include "basalt/math/interval.h"

// Mathematical functions
int x = -5;
auto abs_x = bslt::Abs(x);  // 5

double a = 1.0, b = 1.0000001;
bool equal = bslt::AlmostEqual(a, b, 1e-6);  // true

bool is_power = bslt::IsPowerOfTwo(16);  // true

// Interval operations
bslt::ClosedOpenInterval<int> interval(10, 20);  // [10, 20)

bool contains = interval.Contains(15);  // true
bool contains_end = interval.Contains(20);  // false (exclusive end)

auto midpoint = interval.Midpoint();  // 15
auto length = interval.Length();  // 10

// Interval intersection
auto other = bslt::ClosedOpenInterval<int>(15, 25);  // [15, 25)
auto intersection = interval.Intersection(other);  // [15, 20)

// Formatting
std::string str = absl::StrFormat("%v", interval);  // "[10, 20)"
```

## Dependencies

- [`basalt/base:config`](basalt/base/BUILD.bazel) - Platform and compiler configuration
- [`basalt/type_traits`](basalt/type_traits/BUILD.bazel) - Type trait utilities and concepts
- `@abseil-cpp//absl/hash:hash` - Hash support for intervals
- `@abseil-cpp//absl/strings:str_format` - String formatting support
- `@abseil-cpp//absl/log:check` - Assertion utilities

## Building

The math module is built as part of the Basalt library using Bazel:

```bash
# Build the math library
bazel build //basalt/math:math

# Build the interval library  
bazel build //basalt/math:interval

# Run tests
bazel test //basalt/math:math_test
bazel test //basalt/math:interval_test
```

## Testing

Comprehensive test coverage is provided in:
- [math_test.cc](basalt/math/math_test.cc) - Tests for mathematical functions
- [interval_test.cc](basalt/math/interval_test.cc) - Tests for all interval types and operations

The tests verify correctness across different arithmetic types, edge cases, and boundary conditions.