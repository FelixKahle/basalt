# Basalt Containers

This module provides a header-only implementation of a disjoint, sorted set of closed-open intervals \(`[start, end)`\)
via the `IntervalSet` template. It supports two storage backends:

- `IntervalSet<T, IntervalSetStorage::kVector>` — contiguous \(`std::vector`\) storage optimized for iteration-heavy
  workloads.
- `IntervalSet<T, IntervalSetStorage::kBTree>` — `absl::btree_set` storage optimized for frequent insertions/lookups and
  large numbers of intervals.

## Highlights

- Maintains canonical, disjoint, sorted intervals; adjacent intervals are merged automatically.
- Supports epsilon-tolerant operations for floating point scenarios.
- Header-only API (single header: `basalt/container/interval_set.h`).
- Requires C++20 (ranges, concepts).

## Dependencies

- C++ standard: `-std=c++20`
- Abseil (only for `kBTree` backend): `absl/container/btree_set.h`

## Key types

- `IntervalSet<T, Storage>` — main template
    - `T` — arithmetic endpoint type (int, float, double, ...).
    - `Storage` — `IntervalSetStorage::kVector` (default) or `IntervalSetStorage::kBTree`.

## Common methods (overview)

Constructors:

- `IntervalSet()` — empty
- `IntervalSet(size_type capacity)` — reserve (vector backend)
- `IntervalSet(R&& range)` — bulk-load from a range (merges and sorts)

Insertion / removal:

- `void Insert(IntervalType interval)` — insert and merge
- `void Insert(IntervalType interval, ValueType epsilon)` — with epsilon tolerance
- `void Insert(const IntervalSet& other)` — union
- `void Erase(IntervalType interval)` — subtract interval (may split)
- `void Erase(IntervalType interval, ValueType epsilon)` — epsilon-aware erase
- `void Erase(const IntervalSet& other)` — set difference

Set operations:

- `void Intersection(const IntervalSet& other)`
- Operators: `+=`, `-=`, `&=`, and free `+`, `-`, `&`

Queries:

- `bool Contains(ValueType v)` / `Contains(v, epsilon)`
- `bool ContainsInterval(const IntervalType& other)` / with epsilon
- `bool Intersects(const IntervalType& other)` / with epsilon
- Iteration: `begin()`, `end()`, reverse iterators, `size()`, `clear()`, `IsEmpty()`

Other:

- `void Clip(IntervalType boundary)` — clip set to a boundary interval

## Complexity notes

- Vector backend:
    - Bulk construction: O(N log N) (sort + single-pass merge)
    - Single insert: O(N) worst-case (shifts/erases)
    - Iteration: excellent locality / fast traversal
- BTree backend:
    - Insertion/erase: O(log N) typical for tree ops plus merging logic
    - Good for large numbers of intervals with frequent updates

## Usage example

```cpp
#include "basalt/container/interval_set.h"
#include "basalt/math/interval.h"

using namespace bslt;

int main() 
{
    using IS = IntervalSet<double>;
    IS s;
    s.Insert(ClosedOpenInterval<double>(0.0, 1.0));
    s.Insert(ClosedOpenInterval<double>(1.0, 2.0)); // merged => [0,2)
    bool has = s.Contains(1.5); // true
    s.Erase(ClosedOpenInterval<double>(0.5, 1.5)); // results in [0,0.5) and [1.5,2)
    return has ? 0 : 1;
}
```
