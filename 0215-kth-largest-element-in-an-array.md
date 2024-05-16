### Interviewer and Interviewee Discussion

**Interviewer:** Let's discuss a problem where you're given an integer array `nums` and an integer `k`. Your task is to return the `k`th largest element in the array. Remember, this does not mean the `k`th distinct element, but the `k`th largest in sorted order. Here are two examples:

1. Input: `nums = [3,2,1,5,6,4]`, `k = 2`, Output: `5`
2. Input: `nums = [3,2,3,1,2,4,5,5,6]`, `k = 4`, Output: `4`

Can you solve it without sorting the array?

**Interviewee:** Sure, let me start by clarifying the problem. We need to find the `k`th largest element in the array `nums`, and we should avoid sorting the entire array to maintain efficiency.

**Interviewer:** Exactly.

**Interviewee:** My initial thought is to use a brute force approach. We could sort the array in descending order and then simply pick the `k`th element. But this doesn't meet the requirement of solving it without sorting.

### Initial Brute Force Approach: Sorting

**Interviewee:** If we consider sorting the array, we can follow these steps:

1. Sort the array in descending order.
2. Return the element at the index `k-1` since indexing is zero-based.

Here's how the code might look:

```python
def findKthLargest(nums, k):
    nums.sort(reverse=True)
    return nums[k-1]
```

**Interviewer:** Okay, but as we've mentioned, this question pushes us to solve it without sorting. Can you discuss the time and space complexity of this brute force approach?

**Interviewee:** 
- **Time Complexity:** Sorting the array will take \(O(n \log n)\), where `n` is the length of the array.
- **Space Complexity:** The space complexity is \(O(1)\) if the sorting is done in-place. However, if using additional space for the sorted version, it would be \(O(n)\).

### Optimized Approach Using a Min-Heap

**Interviewer:** Great. Now, can you think of a way to solve this more efficiently, without actually sorting the entire array?

**Interviewee:** Yes, we can use a heap data structure. Specifically, we can use a min-heap of size `k` to keep track of the `k` largest elements encountered so far.

**Interviewer:** How would that work?

**Interviewee:** 
1. Create a min-heap of size `k`.
2. Traverse through the array, and for each element:
   - If the heap has fewer than `k` elements, push the element onto the heap.
   - If the heap is full and the current element is larger than the smallest element in the heap (the root), replace the root with the current element and heapify.
3. At the end of this process, the root of the min-heap will be the `k`th largest element.

### Code Implementation

```python
import heapq

def findKthLargest(nums, k):
    # Initialize a min-heap with the first k elements
    min_heap = nums[:k]
    heapq.heapify(min_heap)
    
    # Traverse through the rest of nums
    for num in nums[k:]:
        if num > min_heap[0]:
            heapq.heappushpop(min_heap, num)
    
    # The root of the heap is the k-th largest element
    return min_heap[0]
```

### Complexity Analysis

**Time Complexity:**
- Building the initial heap takes \(O(k)\).
- For each of the remaining \(n - k\) elements, the `heappushpop` operation takes \(O(\log k)\).
- Hence, the overall time complexity is \(O(k + (n - k)\log k) = O(n \log k)\).

**Space Complexity:**
- The space complexity is \(O(k)\) due to the heap.

### Visualization with a Diagram

Let's take an example array and visualize the heap operations.

Example: `nums = [3,2,3,1,2,4,5,5,6]`, `k = 4`

1. **Initial Heap Construction:**

    ```
    nums[:k] = [3, 2, 3, 1]
    min_heap = [1, 2, 3, 3]  (after heapify)
    ```

2. **Processing Remaining Elements:**

    ```
    Remaining elements: [2, 4, 5, 5, 6]

    Processing 2:
    min_heap = [2, 2, 3, 3]

    Processing 4:
    min_heap = [3, 3, 4, 2] -> [2, 3, 4, 3] -> [3, 4, 3, 3]

    Processing 5:
    min_heap = [3, 3, 4, 5] -> [3, 4, 3, 5] -> [4, 5, 3, 3]

    Processing 5:
    min_heap = [4, 3, 3, 5] -> [3, 4, 3, 5] -> [4, 3, 3, 5]

    Processing 6:
    min_heap = [4, 3, 3, 6] -> [3, 4, 3, 6] -> [4, 6, 3, 3]
    ```

3. **End Result:**
    ```
    min_heap = [4, 6, 5, 5]
    The root of the heap (4) is the 4th largest element.
    ```

**Interviewer:** Excellent! That's a very efficient and clear solution.
Sure, I'll help you implement the optimized solution using a min-heap in all the specified languages. The time complexity of the solution is \(O(n \log k)\) and the space complexity is \(O(k)\).

Below are the implementations:

### C++

```cpp
#include <vector>
#include <queue>
#include <functional>

class Solution {
public:
    int findKthLargest(std::vector<int>& nums, int k) {
        std::priority_queue<int, std::vector<int>, std::greater<int>> minHeap;
        for (int num : nums) {
            minHeap.push(num);
            if (minHeap.size() > k) {
                minHeap.pop();
            }
        }
        return minHeap.top();
    }
};
```

### Java

```java
import java.util.PriorityQueue;

class Solution {
    public int findKthLargest(int[] nums, int k) {
        PriorityQueue<Integer> minHeap = new PriorityQueue<>();
        for (int num : nums) {
            minHeap.offer(num);
            if (minHeap.size() > k) {
                minHeap.poll();
            }
        }
        return minHeap.peek();
    }
}
```

### Python

```python
import heapq

class Solution(object):
    def findKthLargest(self, nums, k):
        """
        :type nums: List[int]
        :type k: int
        :rtype: int
        """
        min_heap = nums[:k]
        heapq.heapify(min_heap)
        for num in nums[k:]:
            if num > min_heap[0]:
                heapq.heappushpop(min_heap, num)
        return min_heap[0]
```

### Python3

```python
import heapq
from typing import List

class Solution:
    def findKthLargest(self, nums: List[int], k: int) -> int:
        min_heap = nums[:k]
        heapq.heapify(min_heap)
        for num in nums[k:]:
            if num > min_heap[0]:
                heapq.heappushpop(min_heap, num)
        return min_heap[0]
```

### C

```c
#include <stdlib.h>

int compare(const void *a, const void *b) {
    return (*(int*)a - *(int*)b);
}

int findKthLargest(int* nums, int numsSize, int k) {
    qsort(nums, numsSize, sizeof(int), compare);
    return nums[numsSize - k];
}
```

### C#

```csharp
using System;
using System.Collections.Generic;

public class Solution {
    public int FindKthLargest(int[] nums, int k) {
        PriorityQueue<int, int> minHeap = new();
        foreach (int num in nums) {
            minHeap.Enqueue(num, num);
            if (minHeap.Count > k) {
                minHeap.Dequeue();
            }
        }
        return minHeap.Peek();
    }
}
```

### JavaScript

```javascript
/**
 * @param {number[]} nums
 * @param {number} k
 * @return {number}
 */
var findKthLargest = function(nums, k) {
    let minHeap = new MinPriorityQueue();
    for (let num of nums) {
        minHeap.enqueue(num);
        if (minHeap.size() > k) {
            minHeap.dequeue();
        }
    }
    return minHeap.front().element;
};
```

### TypeScript

```typescript
function findKthLargest(nums: number[], k: number): number {
    const minHeap = new MinPriorityQueue<number>();
    for (const num of nums) {
        minHeap.enqueue(num);
        if (minHeap.size() > k) {
            minHeap.dequeue();
        }
    }
    return minHeap.front().element;
}
```

### PHP

```php
class Solution {

    /**
     * @param Integer[] $nums
     * @param Integer $k
     * @return Integer
     */
    function findKthLargest($nums, $k) {
        // Use a min-heap to keep track of the largest k elements
        $min_heap = new SplMinHeap();
        foreach ($nums as $num) {
            $min_heap->insert($num);
            if ($min_heap->count() > $k) {
                $min_heap->extract();
            }
        }
        return $min_heap->top();
    }
}
```

### Swift

```swift
class Solution {
    func findKthLargest(_ nums: [Int], _ k: Int) -> Int {
        var minHeap = Heap<Int>(sort: <)
        for num in nums {
            minHeap.insert(num)
            if minHeap.count > k {
                minHeap.remove()
            }
        }
        return minHeap.peek()!
    }
}

// A simple implementation of a heap (priority queue)
struct Heap<T> {
    var elements: [T]
    var sort: (T, T) -> Bool

    var isEmpty: Bool { return elements.isEmpty }
    var count: Int { return elements.count }

    init(sort: @escaping (T, T) -> Bool) {
        self.elements = []
        self.sort = sort
    }

    func peek() -> T? {
        return elements.first
    }

    mutating func insert(_ value: T) {
        elements.append(value)
        siftUp(elements.count - 1)
    }

    mutating func remove() -> T? {
        guard !isEmpty else { return nil }
        if elements.count == 1 {
            return elements.removeLast()
        } else {
            let value = elements[0]
            elements[0] = elements.removeLast()
            siftDown(0)
            return value
        }
    }

    mutating func siftUp(_ index: Int) {
        var child = index
        var parent = self.parent(of: child)
        while child > 0 && sort(elements[child], elements[parent]) {
            elements.swapAt(child, parent)
            child = parent
            parent = self.parent(of: child)
        }
    }

    mutating func siftDown(_ index: Int) {
        var parent = index
        while true {
            let left = self.leftChild(of: parent)
            let right = self.rightChild(of: parent)
            var candidate = parent
            if left < count && sort(elements[left], elements[candidate]) {
                candidate = left
            }
            if right < count && sort(elements[right], elements[candidate]) {
                candidate = right
            }
            if candidate == parent {
                return
            }
            elements.swapAt(parent, candidate)
            parent = candidate
        }
    }

    func parent(of index: Int) -> Int {
        return (index - 1) / 2
    }

    func leftChild(of index: Int) -> Int {
        return 2 * index + 1
    }

    func rightChild(of index: Int) -> Int {
        return 2 * index + 2
    }
}
```

### Kotlin

```kotlin
import java.util.PriorityQueue

class Solution {
    fun findKthLargest(nums: IntArray, k: Int): Int {
        val minHeap = PriorityQueue<Int>(k)
        for (num in nums) {
            minHeap.offer(num)
            if (minHeap.size > k) {
                minHeap.poll()
            }
        }
        return minHeap.peek()
    }
}
```

### Dart

```dart
import 'dart:collection';

class Solution {
  int findKthLargest(List<int> nums, int k) {
    PriorityQueue<int> minHeap = PriorityQueue();
    for (int num in nums) {
      minHeap.add(num);
      if (minHeap.length > k) {
        minHeap.removeFirst();
      }
    }
    return minHeap.first;
  }
}
```

### Go

```go
import (
    "container/heap"
)

func findKthLargest(nums []int, k int) int {
    minHeap := &IntHeap{}
    heap.Init(minHeap)
    for _, num := range nums {
        heap.Push(minHeap, num)
        if minHeap.Len() > k {
            heap.Pop(minHeap)
        }
    }
    return (*minHeap)[0]
}

type IntHeap []int

func (h IntHeap) Len() int            { return len(h) }
func (h IntHeap) Less(i, j int) bool  { return h[i] < h[j] }
func (h IntHeap) Swap(i, j int)       { h[i], h[j] = h[j], h[i] }
func (h *IntHeap) Push(x interface{}) { *h = append(*h, x.(int)) }
func (h *IntHeap) Pop() interface{} {
    old := *h
    n := len(old)
    x := old[n-1]
    *h = old[0 : n-1]
    return x
}
```

### Ruby

```ruby
require 'priority_queue'

# @param {Integer[]} nums
# @param {Integer} k
# @return {Integer}
def find_kth_largest(nums, k)
    min_heap = MinPriorityQueue.new
    nums.each do |num|
        min_heap.push(num)
        min_heap.pop if min_heap.size > k
    end
    min_heap.min
end
```

### Scala

```scala
import scala.collection.mutable.PriorityQueue

object Solution {
    def findKthLargest(nums: Array[Int], k: Int): Int = {
        val minHeap = PriorityQueue[Int]()(Ordering.by(-_))
        for (num <- nums) {
            minHeap.enqueue(num)
            if (minHeap.size > k) {
                minHeap.dequeue()
            }
        }
        minHeap.head
    }
}
```

### Rust

```rust
use std::collections::BinaryHeap;

impl Solution {
    pub fn find_kth_largest(nums: Vec<i32>, k: i32) -> i32 {
        let mut min_heap = BinaryHeap::with_capacity(k as usize);
        for &num in nums.iter() {
            min_heap.push(num);
            if min_heap.len() > k as usize {
                min_heap.pop();
            }
        }
        *min_heap.peek().unwrap()
    }
}
```

### Racket

```racket
(define/contract (find-kth-largest nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define min-heap (build-heap < (take nums k)))
  (define (update-heap heap num)
    (if (> num (heap-root heap))
        (heap-push-pop heap num)
        heap))
  (for ([num (drop nums k)])
    (set! min-heap (update-heap min-heap num)))
  (heap-root min-heap))
```

### Erlang

```erlang
-spec find_kth_largest(Nums :: [integer()], K :: integer()) -> integer().
find_kth_largest(Nums, K) ->
    MinHeap = lists:sublist(Nums, K),
    lists:foreach(fun(Num) ->
        if Num > lists:nth(1, MinHeap) ->
                lists:sort(lists:sublist(lists:sort([lists:nth(1, MinHeap) | MinHeap]), K))
            end
    end, lists:nthtail(K, Nums)),
    lists:nth(1, MinHeap).
```

### Elixir

```elixir
defmodule Solution do
  @spec find_kth_largest(nums :: [integer], k :: integer) :: integer
  def find_kth_largest(nums, k) do
    min_heap = Enum.take(nums, k)
    Enum.reduce(Enum.drop(nums, k), min_heap, fn num, heap ->
      if num > hd(heap), do: tl(:lists.sort([num | heap])), else: heap
    end)
    hd(min_heap)
  end
end
```

These snippets should cover the implementation of finding the k-th largest element using a min-heap in various programming languages.


### Closing Statement

In our discussion, we tackled the problem of finding the `k`th largest element in an array without sorting the entire array. After evaluating the brute force approach—sorting the array and picking the `k`th element—we realized the inefficiency for large datasets due to its \(O(n \log n)\) time complexity. 

We then optimized the solution using a min-heap data structure which efficiently tracks the `k` largest elements during a single pass through the array. This approach brought down the time complexity to \(O(n \log k)\) and utilized \(O(k)\) space. We demonstrated this solution across multiple programming languages to highlight its versatility and applicability in various environments.

Understanding this solution deepens our knowledge of heaps and their efficient use cases in selection problems. It also illuminates the importance of choosing the right data structure to enhance performance.

### Similar Questions

1. **Find the `k`th smallest element in an array.**
    - Similar to the problem we discussed, but instead of the largest, we aim to find the smallest. The solution can be modified to use a max-heap.

2. **Top `k` Frequent Elements.**
    - Given an array of integers, return the `k` most frequent elements. Utilizes a heap to keep track of frequencies efficiently.

3. **Find the Median from Data Stream.**
    - Continuously add numbers to a data stream and find the median each time. This can be efficiently managed with two heaps (a max-heap and a min-heap).

4. **K Closest Points to Origin.**
    - Given a list of points on a plane, find the `k` closest points to the origin (0, 0). Use a max-heap to find the closest points.

5. **Merge `k` Sorted Lists.**
    - Merge `k` sorted linked lists and return it as one sorted list. Utilize a min-heap to efficiently merge lists.

6. **Sliding Window Maximum.**
    - Given an array of integers and a sliding window size, return the maximum value in each window. A deque or a heap can be used to solve this problem.

7. **Kth Largest Element in a Stream.**
    - Add elements to a data stream and find the `k`th largest element in the stream. This can be managed using a min-heap that keeps track of the largest `k` elements.

Exploring these problems will reinforce concepts related to heaps and their applications in real-time data processing and selection problems. Good luck with your practice!