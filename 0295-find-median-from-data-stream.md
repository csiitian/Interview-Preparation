### Interviewer and Interviewee Discussion

**Interviewer:** Let's start by discussing how you would approach the given problem. We're asked to implement a `MedianFinder` class which should be able to add numbers from a data stream and find the median of all elements so far. How would you begin tackling this problem?

**Interviewee:** The core of our task is to efficiently maintain a collection of numbers so that we can quickly determine the median. Initially, we can think about the brute force approach where every time a new number is added, we insert it into the collection, keep the collection sorted, and then compute the median from this sorted list.

**Interviewer:** That sounds like a reasonable start. Could you walk through the brute force approach in detail?

### Initial Brute Force Approach

**Interviewee:** Sure! Here’s what we could do:

1. **Initialization**: Start with an empty data structure, typically a list.
2. **Adding Numbers**: Whenever a new number is added, insert it in the sorted order in the list.
3. **Finding the Median**: To find the median, check the length of the list:
   - If the length is odd, the median is the middle element.
   - If the length is even, the median is the average of the two middle elements.
   
Here is a straightforward implementation:

**Python Code:**
```python
class MedianFinder:
    def __init__(self):
        self.nums = []
    
    def addNum(self, num: int) -> None:
        # Insert num in sorted order
        self.nums.append(num)
        self.nums.sort()
    
    def findMedian(self) -> float:
        n = len(self.nums)
        if n % 2 == 1:
            return float(self.nums[n // 2])
        else:
            mid1 = self.nums[(n // 2) - 1]
            mid2 = self.nums[n // 2]
            return (mid1 + mid2) / 2.0
```

### Time and Space Complexity of Brute Force

**Interviewer:** What are the time and space complexities of this initial solution?

**Interviewee:**

- **Time Complexity**:
  - **addNum**: O(n log n) because `self.nums.sort()` is called every time we add a number.
  - **findMedian**: O(1) as it involves simple index calculations and conditional checks.
  
- **Space Complexity**: O(n) because we store all the elements added to the list.

### Optimizing the Approach

**Interviewer:** The brute force approach is simple, but sorting after every insertion is expensive. How can we optimize this?

**Interviewee:** We can use two heaps (a max-heap for the left half and a min-heap for the right half) to maintain a balance. This approach ensures that the median can be found in logarithmic time with logarithmic insertion complexity.

**Interviewer:** Can you explain how the heaps would be used to optimize the solution?

**Interviewee:** Certainly. Here’s the improved approach:

1. **Use Two Heaps**:
   - **Max-Heap** (left): Contains the smaller half of the numbers.
   - **Min-Heap** (right): Contains the larger half of the numbers.

2. **Balancing**:
   - Always keep the heaps balanced such that the difference in their sizes is no more than one.
   - Max-heap can have one extra element more than the min-heap when the total count is odd.

3. **Finding the Median**:
   - If both heaps are of the same size, the median will be the average of the top elements of both heaps.
   - If the max-heap has one extra element, it contains the median directly.

**Python Code with Heaps:**

```python
import heapq

class MedianFinder:
    def __init__(self):
        self.small = []  # Max-Heap (invert sign for max-heap using min-heap)
        self.large = []  # Min-Heap
    
    def addNum(self, num: int) -> None:
        heapq.heappush(self.small, -num)
        # Ensure the largest of small is smaller than the smallest of large
        if self.small and self.large and (-self.small[0] > self.large[0]):
            heapq.heappush(self.large, -heapq.heappop(self.small))
        
        # Balance the sizes of the heaps
        if len(self.small) > len(self.large) + 1:
            heapq.heappush(self.large, -heapq.heappop(self.small))
        if len(self.large) > len(self.small):
            heapq.heappush(self.small, -heapq.heappop(self.large))
    
    def findMedian(self) -> float:
        if len(self.small) > len(self.large):
            return -self.small[0]
        return (-self.small[0] + self.large[0]) / 2.0
```

### Time and Space Complexity of Optimized Solution

**Interviewer:** How does this optimized approach compare in terms of time and space complexity?

**Interviewee:**

- **Time Complexity**: 
  - **addNum**: O(log n) for balancing the heaps.
  - **findMedian**: O(1) for looking at the top elements of the heaps.

- **Space Complexity**: O(n) to maintain the heaps.

### Illustrated Explanation

**Interviewer:** It would be helpful to visualize this approach. Can you draw an explanation for the heap-based solution?

**Interviewee:** Here’s an illustration:

```
Example Input: 1, 2, 3
Heaps representation after each insertion:

1. addNum(1)
   small: [1]
   large: []

2. addNum(2)
   small: [1]
   large: [2]

3. addNum(3)
   small: [1]
   large: [2, 3]

The heaps state:
  - small: [1]
  - large: [2, 3]

Finding Median:
- Since the heaps are of even sizes, the median is the average of -small[0] and large[0]:
  median = (1 + 2) / 2 = 1.5
```

**Interviewer:** Great, that explanation is clear and precise. You’ve illustrated how the heaps are balanced and the median is found efficiently.

**Interviewee:** Thank you! This optimized approach should handle the constraints effectively.
Sure, I'll integrate the optimized heap-based approach into each of the provided code snippets for different programming languages. Here's how the code would look:

### C++

```cpp
#include <queue>
using namespace std;

class MedianFinder {
public:
    priority_queue<int> small;                     // max-heap (lower half)
    priority_queue<int, vector<int>, greater<int>> large; // min-heap (upper half)

    MedianFinder() {
    }

    void addNum(int num) {
        small.push(num);
        if (!small.empty() && !large.empty() && small.top() > large.top()) {
            large.push(small.top());
            small.pop();
        }

        if (small.size() > large.size() + 1) {
            large.push(small.top());
            small.pop();
        }
        
        if (large.size() > small.size()) {
            small.push(large.top());
            large.pop();
        }
    }

    double findMedian() {
        if (small.size() > large.size())
            return small.top();
        return (small.top() + large.top()) / 2.0;
    }
};

/**
 * Your MedianFinder object will be instantiated and called as such:
 * MedianFinder* obj = new MedianFinder();
 * obj->addNum(num);
 * double param_2 = obj->findMedian();
 */
```

### Java

```java
import java.util.*;

class MedianFinder {

    private PriorityQueue<Integer> small; // max-heap (lower half)
    private PriorityQueue<Integer> large; // min-heap (upper half)

    public MedianFinder() {
        small = new PriorityQueue<>(Collections.reverseOrder());
        large = new PriorityQueue<>();
    }

    public void addNum(int num) {
        small.offer(num);
        if (!small.isEmpty() && !large.isEmpty() && small.peek() > large.peek()) {
            large.offer(small.poll());
        }

        if (small.size() > large.size() + 1) {
            large.offer(small.poll());
        }
        
        if (large.size() > small.size()) {
            small.offer(large.poll());
        }
    }

    public double findMedian() {
        if (small.size() > large.size()) {
            return small.peek();
        }
        return (small.peek() + large.peek()) / 2.0;
    }
}

/**
 * Your MedianFinder object will be instantiated and called as such:
 * MedianFinder obj = new MedianFinder();
 * obj.addNum(num);
 * double param_2 = obj.findMedian();
 */
```

### Python

```python
import heapq

class MedianFinder(object):

    def __init__(self):
        self.small = []  # Max-Heap (invert sign for max-heap using min-heap)
        self.large = []  # Min-Heap

    def addNum(self, num):
        """
        :type num: int
        :rtype: None
        """
        heapq.heappush(self.small, -num)
        if self.small and self.large and (-self.small[0] > self.large[0]):
            heapq.heappush(self.large, -heapq.heappop(self.small))
        
        if len(self.small) > len(self.large) + 1:
            heapq.heappush(self.large, -heapq.heappop(self.small))
        if len(self.large) > len(self.small):
            heapq.heappush(self.small, -heapq.heappop(self.large))

    def findMedian(self):
        """
        :rtype: float
        """
        if len(self.small) > len(self.large):
            return -self.small[0]
        return (-self.small[0] + self.large[0]) / 2.0


# Your MedianFinder object will be instantiated and called as such:
# obj = MedianFinder()
# obj.addNum(num)
# param_2 = obj.findMedian()
```

### Python3

```python
import heapq

class MedianFinder:

    def __init__(self):
        self.small = []  # Max-Heap (invert sign for max-heap using min-heap)
        self.large = []  # Min-Heap

    def addNum(self, num: int) -> None:
        heapq.heappush(self.small, -num)
        if self.small and self.large and (-self.small[0] > self.large[0]):
            heapq.heappush(self.large, -heapq.heappop(self.small))
        
        if len(self.small) > len(self.large) + 1:
            heapq.heappush(self.large, -heapq.heappop(self.small))
        if len(self.large) > len(self.small):
            heapq.heappush(self.small, -heapq.heappop(self.large))

    def findMedian(self) -> float:
        if len(self.small) > len(self.large):
            return -self.small[0]
        return (-self.small[0] + self.large[0]) / 2.0


# Your MedianFinder object will be instantiated and called as such:
# obj = MedianFinder()
# obj.addNum(num)
# param_2 = obj.findMedian()
```

### C

```c
#include <stdlib.h>
#include <stdio.h>
#include <limits.h>

typedef struct {
    int smallSize;
    int largeSize;
    int small[50001];
    int large[50001];
} MedianFinder;

MedianFinder* medianFinderCreate() {
    MedianFinder* obj = (MedianFinder*)malloc(sizeof(MedianFinder));
    obj->smallSize = 0;
    obj->largeSize = 0;
    return obj;
}

void medianFinderAddNum(MedianFinder* obj, int num) {
    obj->small[obj->smallSize++] = num; 
    for (int i = obj->smallSize / 2 - 1; i >= 0; --i) {
        int n = (i * 2 + 1 < obj->smallSize && obj->small[i] < obj->small[i * 2 + 1]) ? (i * 2 + 1) : i;
        n = (i * 2 + 2 < obj->smallSize && obj->small[n] < obj->small[i * 2 + 2]) ? (i * 2 + 2) : n;
        if (n == i) return;
        int tmp = obj->small[i];
        obj->small[i] = obj->small[n];
        obj->small[n] = tmp;
    }
    
    if (obj->smallSize > 1) {
        while (obj->small[0] < obj->small[1]) {
            int tmp = obj->small[0];
            obj->small[0] = obj->small[1];
            obj->small[1] = tmp;
        }
    }
    
    if (obj->largeSize > 0 && obj->small[0] > obj->large[0]) {
        int tmp = obj->small[0];
        obj->small[0] = obj->large[0];
        obj->large[0] = tmp;
    }
    
    while (obj->smallSize > obj->largeSize + 1) {
        obj->large[obj->largeSize++] = obj->small[--obj->smallSize];
        for (int i = obj->largeSize / 2 - 1; i >= 0; --i) {
            int n = (i * 2 + 1 < obj->largeSize && obj->large[i] > obj->large[i * 2 + 1]) ? (i * 2 + 1) : i;
            n = (i * 2 + 2 < obj->largeSize && obj->large[n] > obj->large[i * 2 + 2]) ? (i * 2 + 2) : n;
            if (n == i) return;
            int tmp = obj->large[i];
            obj->large[i] = obj->large[n];
            obj->large[n] = tmp;
        }
    }
}

double medianFinderFindMedian(MedianFinder* obj) {
    if (obj->smallSize > obj->largeSize) return obj->small[0];
    return (obj->small[0] + obj->large[0]) / 2.0;
}

void medianFinderFree(MedianFinder* obj) {
    free(obj);
}

/**
 * Your MedianFinder struct will be instantiated and called as such:
 * MedianFinder* obj = medianFinderCreate();
 * medianFinderAddNum(obj, num);
 * double param_2 = medianFinderFindMedian(obj);
 * medianFinderFree(obj);
*/
```

### C#

```csharp
using System;
using System.Collections.Generic;

public class MedianFinder {

    private PriorityQueue<int, int> small; // max-heap (lower half)
    private PriorityQueue<int, int> large; // min-heap (upper half)

    public MedianFinder() {
        small = new PriorityQueue<int, int>(Comparer<int>.Create((x, y) => y.CompareTo(x)));
        large = new PriorityQueue<int, int>();
    }

    public void AddNum(int num) {
        small.Enqueue(num, num);
        if (small.Count > 0 && large.Count > 0 && small.Peek() > large.Peek()) {
            large.Enqueue(small.Dequeue(), small.Peek());
        }
        
        if (small.Count > large.Count + 1) {
            large.Enqueue(small.Dequeue(), small.Peek());
        }
        
        if (large.Count > small.Count) {
            small.Enqueue(large.Dequeue(), large.Peek());
        }
    }

    public double FindMedian() {
        if (small.Count > large.Count) {
            return small.Peek();
        }
        return (small.Peek() + large.Peek()) / 2.0;
    }
}

/**
 * Your MedianFinder object will be instantiated and called as such:
 * MedianFinder obj = new MedianFinder();
 * obj.AddNum(num);
 * double param_2 = obj.FindMedian();
 */
```

### JavaScript

```javascript
class MedianFinder {
    constructor() {
        this.small = []; // max-heap (lower half)
        this.large = []; // min-heap (upper half)
    }

    addNum(num) {
        this.small.push(num);
        this.small.sort((a, b) => b - a);
        
        if (this.small.length > 0 && this.large.length > 0 && this.small[0] > this.large[0]) {
            this.large.push(this.small.shift());
        }
        
        if (this.small.length > this.large.length + 1) {
            this.large.push(this.small.shift());
        }
        
        if (this.large.length > this.small.length) {
            this.small.push(this.large.shift());
        }
    }

    findMedian() {
        if (this.small.length > this.large.length) {
            return this.small[0];
        }
        return (this.small[0] + this.large[0]) / 2.0;
    }
}

/**
 * Your MedianFinder object will be instantiated and called as such:
 * var obj = new MedianFinder()
 * obj.addNum(num)
 * var param_2 = obj.findMedian()
 */
```

### TypeScript

```typescript
class MedianFinder {
    small: number[]; // max-heap (lower half)
    large: number[]; // min-heap (upper half)

    constructor() {
        this.small = [];
        this.large = [];
    }

    addNum(num: number): void {
        this.small.push(num);
        this.small.sort((a, b) => b - a);
        
        if (this.small.length > 0 && this.large.length > 0 && this.small[0] > this.large[0]) {
            this.large.push(this.small.shift());
        }
        
        if (this.small.length > this.large.length + 1) {
            this.large.push(this.small.shift());
        }
        
        if (this.large.length > this.small.length) {
            this.small.push(this.large.shift());
        }
    }

    findMedian(): number {
        if (this.small.length > this.large.length) {
            return this.small[0];
        }
        return (this.small[0] + this.large[0]) / 2.0;
    }
}

/**
 * Your MedianFinder object will be instantiated and called as such:
 * var obj = new MedianFinder()
 * obj.addNum(num)
 * var param_2 = obj.findMedian()
 */
```

### PHP

```php
class MedianFinder {
    private $small; // max-heap (lower half)
    private $large; // min-heap (upper half)

    function __construct() {
        $this->small = new SplMaxHeap();
        $this->large = new SplMinHeap();
    }
  
    function addNum($num) {
        $this->small->insert($num);
        if (!$this->small->isEmpty() && !$this->large->isEmpty() && $this->small->top() > $this->large->top()) {
            $this->large->insert($this->small->extract());
        }

        if ($this->small->count() > $this->large->count() + 1) {
            $this->large->insert($this->small->extract());
        }
        
        if ($this->large->count() > $this->small->count()) {
            $this->small->insert($this->large->extract());
        }
    }
  
    function findMedian() {
        if ($this->small->count() > $this->large->count()) {
            return $this->small->top();
        }
        return ($this->small->top() + $this->large->top()) / 2.0;
    }
}

/**
 * Your MedianFinder object will be instantiated and called as such:
 * $obj = MedianFinder();
 * $obj->addNum($num);
 * $ret_2 = $obj->findMedian();
 */
```

### Swift

```swift
import Foundation

class MedianFinder {

    var small = Heap<Int>(sort: >) // max-heap (lower half)
    var large = Heap<Int>(sort: <) // min-heap (upper half)

    init() {
        
    }
    
    func addNum(_ num: Int) {
        small.insert(num)
        if !small.isEmpty && !large.isEmpty && small.peek()! > large.peek()! {
            large.insert(small.remove()!)
        }
        
        if small.count > large.count + 1 {
            large.insert(small.remove()!)
        }
        
        if large.count > small.count {
            small.insert(large.remove()!)
        }
    }
    
    func findMedian() -> Double {
        if small.count > large.count {
            return Double(small.peek()!)
        }
        return Double(small.peek()! + large.peek()!) / 2.0
    }
}

// Heap class
struct Heap<Element: Equatable> {
    var elements: [Element]
    let sort: (Element, Element) -> Bool
  
    init(sort: @escaping (Element, Element) -> Bool) {
        self.sort = sort
        self.elements = []
    }
  
    var isEmpty: Bool {
        return elements.isEmpty
    }
  
    var count: Int {
        return elements.count
    }
  
    func peek() -> Element? {
        return elements.first
    }
  
    mutating func insert(_ value: Element) {
        elements.append(value)
        siftUp(from: elements.count - 1)
    }
  
    mutating func remove() -> Element? {
        guard !elements.isEmpty else {
            return nil
        }
        if elements.count == 1 {
            return elements.removeLast()
        } else {
            let value = elements.first
            elements[0] = elements.removeLast()
            siftDown(from: 0)
            return value
        }
    }
  
    private mutating func siftUp(from index: Int) {
        var childIndex = index
        var parentIndex = self.parentIndex(of: childIndex)
        while childIndex > 0 && sort(elements[childIndex], elements[parentIndex]) {
            elements.swapAt(childIndex, parentIndex)
            childIndex = parentIndex
            parentIndex = self.parentIndex(of: childIndex)
        }
    }
  
    private mutating func siftDown(from index: Int) {
        var parentIndex = index
        while true {
            let leftChildIndex = self.leftChildIndex(of: parentIndex)
            let rightChildIndex = self.rightChildIndex(of: parentIndex)
            var optionalParentSwapIndex: Int?
          
            if leftChildIndex < count && sort(elements[leftChildIndex], elements[parentIndex]) {
                optionalParentSwapIndex = leftChildIndex
            }
            if rightChildIndex < count {
                if let parentSwapIndex = optionalParentSwapIndex {
                    if sort(elements[rightChildIndex], elements[parentSwapIndex]) {
                        optionalParentSwapIndex = rightChildIndex
                    }
                } else if sort(elements[rightChildIndex], elements[parentIndex]) {
                    optionalParentSwapIndex = rightChildIndex
                }
            }
            guard let parentSwapIndex = optionalParentSwapIndex else {
                return
            }
            elements.swapAt(parentIndex, parentSwapIndex)
            parentIndex = parentSwapIndex
        }
    }
  
    private func parentIndex(of index: Int) -> Int {
        return (index - 1) / 2
    }
  
    private func leftChildIndex(of index: Int) -> Int {
        return 2 * index + 1
    }
  
    private func rightChildIndex(of index: Int) -> Int {
        return 2 * index + 2
    }
}

/**
 * Your MedianFinder object will be instantiated and called as such:
 * let obj = MedianFinder()
 * obj.addNum(num)
 * let ret_2: Double = obj.findMedian()
 */
```

### Kotlin

```kotlin
import java.util.*

class MedianFinder() {

    private val small = PriorityQueue<Int>(Collections.reverseOrder()) // max-heap (lower half)
    private val large = PriorityQueue<Int>() // min-heap (upper half)

    fun addNum(num: Int) {
        small.offer(num)
        if (!small.isEmpty() && !large.isEmpty() && small.peek() > large.peek()) {
            large.offer(small.poll())
        }
        
        if (small.size > large.size + 1) {
            large.offer(small.poll())
        }
        
        if (large.size > small.size) {
            small.offer(large.poll())
        }
    }

    fun findMedian(): Double {
        return if (small.size > large.size) {
            small.peek().toDouble()
        } else {
            (small.peek().toDouble() + large.peek().toDouble()) / 2.0
        }
    }
}

/**
 * Your MedianFinder object will be instantiated and called as such:
 * var obj = MedianFinder()
 * obj.addNum(num)
 * var param_2 = obj.findMedian()
 */
```



### Closing Statement

In this discussion, we explored the problem of maintaining a data stream and efficiently calculating the median of the elements seen so far. We started with a brute-force approach involving sorting and identified its time complexity limitations. We then optimized our approach using two heaps (max-heap and min-heap) to keep the smaller and larger halves of the elements balanced, allowing us to find the median efficiently. We provided implementations of this optimized solution across multiple programming languages, showcasing versatility and adaptability in different environments. Understanding and implementing efficient algorithms for median finding is a valuable skill, particularly important in scenarios involving continuous data streams or large datasets.

### Similar Questions

If you found the MedianFinder problem interesting, you may want to try solving the following related problems as well:

1. **Sliding Window Median**:
   - **Description**: Given an array of `n` integers, there is a sliding window of size `k` which is moving from the very left of the array to the very right. Find the median of the elements inside the window for each window position.
   - **LeetCode Link**: [Sliding Window Median](https://leetcode.com/problems/sliding-window-median/)

2. **Find K-th Largest Element in an Array**:
   - **Description**: Given an unsorted array, find the k-th largest element in it.
   - **LeetCode Link**: [Kth Largest Element in an Array](https://leetcode.com/problems/kth-largest-element-in-an-array/)

3. **Data Stream as Disjoint Intervals**:
   - **Description**: Given a data stream input of non-negative integers a1, a2, ..., an, ..., summarize the numbers seen so far as a list of disjoint intervals.
   - **LeetCode Link**: [Data Stream as Disjoint Intervals](https://leetcode.com/problems/data-stream-as-disjoint-intervals/)

4. **Find Median from Data Stream using Binary Search**:
   - **Description**: Implement a more advanced version of median finding using balanced binary search trees or other advanced data structures.
   - **LeetCode Link**: (You can create your own problem statement based on combining the concept of trees with data streams.)

5. **Longest Continuous Subarray with Absolute Diff Less Than or Equal to Limit**:
   - **Description**: Given an array of integers nums and an integer limit, return the size of the longest continuous subarray such that the absolute difference between any two elements is less than or equal to the limit.
   - **LeetCode Link**: [Longest Continuous Subarray with Absolute Diff Less Than or Equal to Limit](https://leetcode.com/problems/longest-continuous-subarray-with-absolute-diff-less-than-or-equal-to-limit/)

Exploring these problems will not only help reinforce the concepts you've learned but also broaden your understanding of how to manage and process data streams efficiently. Happy coding!