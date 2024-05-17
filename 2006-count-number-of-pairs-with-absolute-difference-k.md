### Interviewer and Interviewee Discussion

#### Interviewer:
Alright, let's discuss the problem at hand. You have an integer array `nums` and an integer `k`. Your task is to return the number of pairs `(i, j)` where `i < j` such that `|nums[i] - nums[j]| == k`. Do you understand the problem?

#### Interviewee:
Yes, I understand. We need to find pairs of indices `(i, j)` where `i` is less than `j` and the absolute difference between `nums[i]` and `nums[j]` is exactly `k`.

#### Interviewer:
Great! Let's start with an initial brute force approach. How would you tackle this problem?

#### Interviewee:
For a brute force solution, I would:

1. Iterate over each element in the array using a nested loop.
2. For each element `nums[i]`, I would check every subsequent element `nums[j]` where `j > i`.
3. If the absolute difference `|nums[i] - nums[j]|` is equal to `k`, I increment a counter.
4. Finally, return the counter.

Here is the pseudo-code for that approach:

```python
count = 0
for i in range(len(nums)):
    for j in range(i + 1, len(nums)):
        if abs(nums[i] - nums[j]) == k:
            count += 1
return count
```

#### Interviewer:
That makes sense. Can you analyze the time and space complexity of this brute force approach?

#### Interviewee:
Sure! 

- **Time Complexity:** The outer loop runs `n` times, and for each iteration of the outer loop, the inner loop runs up to `n-1` times. This results in an overall time complexity of \(O(n^2)\).
  
- **Space Complexity:** We only use a few extra variables for counting, so the space complexity is \(O(1)\).

### Drawing to Explain Brute Force Approach
Let's visualize this algorithm with a small example:

Consider `nums = [1, 2, 2, 1]` and `k = 1`.

1. Start with `i = 0` and check pairs `(0, j)` where `j` runs from `1` to `3`.
    - `(0, 1)` -> `|1 - 2| = 1` (count increases to 1)
    - `(0, 2)` -> `|1 - 2| = 1` (count increases to 2)
    - `(0, 3)` -> `|1 - 1| = 0` (not counted)

2. Move to `i = 1` and check pairs `(1, j)` where `j` runs from `2` to `3`.
    - `(1, 2)` -> `|2 - 2| = 0` (not counted)
    - `(1, 3)` -> `|2 - 1| = 1` (count increases to 3)

3. Finally, `i = 2` and check pair `(2, 3)`:
    - `(2, 3)` -> `|2 - 1| = 1` (count increases to 4)

Thus, the final count is 4.

### Optimization using Hash Map

#### Interviewer:
Can we optimize this further to reduce the time complexity?

#### Interviewee:
Yes, we can use a hashmap (or dictionary) to keep track of the occurrences of elements in `nums`. This will allow us to efficiently check for pairs with a difference of `k`. Here's how:

1. Create a hashmap to store the frequency of each element in the array.
2. Iterate over the array. For each element `num`, check if `num + k` and `num - k` are in the hashmap.
3. If either of them exists, increment the count by the frequency of those numbers in the hashmap.
4. Update the hashmap to include the current element `num`.

Here's the pseudo-code for this optimized approach:

```python
from collections import defaultdict

def count_pairs(nums, k):
    count = 0
    freq = defaultdict(int)
    
    for num in nums:
        if (num + k) in freq:
            count += freq[num + k]
        if (num - k) in freq:
            count += freq[num - k]
        
        freq[num] += 1
    
    return count
```

#### Interviewer:
Great! Can you analyze the time and space complexity of this optimized approach?

#### Interviewee:
Sure.

- **Time Complexity:** We iterate over the array once, which is \(O(n)\). Checking and updating the hashmap operations are \(O(1)\) on average. So the overall time complexity is \(O(n)\).

- **Space Complexity:** We use a hashmap to store the frequencies of elements, which in the worst case can have \(O(n)\) space complexity.

### Drawing to Explain Optimized Approach
Let's visualize the optimized algorithm with `nums = [1, 2, 2, 1]` and `k = 1`.

1. Initialize `freq` as an empty dictionary and `count` as 0.
2. Iterate over each number while updating the dictionary and counting pairs:
    - `num = 1`:
        - Check for `1 + 1 (2)` and `1 - 1 (0)`: both are not in `freq`.
        - Update `freq` to `{1: 1}`.
    - `num = 2`:
        - Check for `2 + 1 (3)` and `2 - 1 (1)`: `1` is in `freq` with count 1 (increment `count` to 1).
        - Update `freq` to `{1: 1, 2: 1}`.
    - `num = 2`:
        - Check for `2 + 1 (3)` and `2 - 1 (1)`: `1` is in `freq` with count 1 (increment `count` to 2).
        - Update `freq` to `{1: 1, 2: 2}`.
    - `num = 1`:
        - Check for `1 + 1 (2)` and `1 - 1 (0)`: `2` is in `freq` with count 2 (increment `count` to 4).
        - Update `freq` to `{1: 2, 2: 2}`.

Thus, the final count is 4, which matches our previous results but with better efficiency.
Sure, here is the optimized solution implemented in various programming languages, taking into consideration the time and space complexity as well.

### C++

```cpp
#include <vector>
#include <unordered_map>

class Solution {
public:
    int countKDifference(std::vector<int>& nums, int k) {
        std::unordered_map<int, int> freq;
        int count = 0;
        
        for (auto num : nums) {
            if (freq.find(num + k) != freq.end()) {
                count += freq[num + k];
            }
            if (freq.find(num - k) != freq.end()) {
                count += freq[num - k];
            }
            freq[num]++;
        }
        return count;
    }
};
```

### Java

```java
import java.util.HashMap;
import java.util.Map;

class Solution {
    public int countKDifference(int[] nums, int k) {
        Map<Integer, Integer> freq = new HashMap<>();
        int count = 0;
        
        for (int num : nums) {
            count += freq.getOrDefault(num + k, 0);
            count += freq.getOrDefault(num - k, 0);
            freq.put(num, freq.getOrDefault(num, 0) + 1);
        }
        return count;
    }
}
```

### Python

```python
class Solution(object):
    def countKDifference(self, nums, k):
        """
        :type nums: List[int]
        :type k: int
        :rtype: int
        """
        from collections import defaultdict
        
        freq = defaultdict(int)
        count = 0
        
        for num in nums:
            count += freq[num + k]
            count += freq[num - k]
            freq[num] += 1
        
        return count
```

### Python3

```python
class Solution:
    def countKDifference(self, nums: List[int], k: int) -> int:
        from collections import defaultdict
        
        freq = defaultdict(int)
        count = 0
        
        for num in nums:
            count += freq[num + k]
            count += freq[num - k]
            freq[num] += 1
        
        return count
```

### C

```c
#include <stdlib.h>

int countKDifference(int* nums, int numsSize, int k) {
    int* freq = (int*)calloc(201, sizeof(int));  // Allocate memory for frequency array
    int count = 0;
    
    for (int i = 0; i < numsSize; ++i) {
        if (nums[i] + k <= 100) {
            count += freq[nums[i] + k];
        }
        if (nums[i] - k >= 1) {
            count += freq[nums[i] - k];
        }
        freq[nums[i]]++;
    }
    
    free(freq);  // Free the allocated memory
    return count;
}
```

### C#

```csharp
using System.Collections.Generic;

public class Solution {
    public int CountKDifference(int[] nums, int k) {
        Dictionary<int, int> freq = new Dictionary<int, int>();
        int count = 0;
        
        foreach (int num in nums) {
            if (freq.ContainsKey(num + k)) {
                count += freq[num + k];
            }
            if (freq.ContainsKey(num - k)) {
                count += freq[num - k];
            }
            if (!freq.ContainsKey(num)) {
                freq[num] = 0;
            }
            freq[num]++;
        }
        return count;
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
var countKDifference = function(nums, k) {
    const freq = new Map();
    let count = 0;

    for (const num of nums) {
        count += (freq.get(num + k) || 0);
        count += (freq.get(num - k) || 0);
        freq.set(num, (freq.get(num) || 0) + 1);
    }

    return count;
};
```

### TypeScript

```typescript
function countKDifference(nums: number[], k: number): number {
    const freq = new Map<number, number>();
    let count = 0;

    for (const num of nums) {
        count += (freq.get(num + k) || 0);
        count += (freq.get(num - k) || 0);
        freq.set(num, (freq.get(num) || 0) + 1);
    }

    return count;
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
    function countKDifference($nums, $k) {
        $freq = [];
        $count = 0;
        
        foreach ($nums as $num) {
            if (isset($freq[$num + $k])) {
                $count += $freq[$num + $k];
            }
            if (isset($freq[$num - $k])) {
                $count += $freq[$num - $k];
            }
            if (isset($freq[$num])) {
                $freq[$num]++;
            } else {
                $freq[$num] = 1;
            }
        }
        
        return $count;
    }
}
```

### Swift

```swift
class Solution {
    func countKDifference(_ nums: [Int], _ k: Int) -> Int {
        var freq = [Int: Int]()
        var count = 0
        
        for num in nums {
            if let val = freq[num + k] {
                count += val
            }
            if let val = freq[num - k] {
                count += val
            }
            freq[num, default: 0] += 1
        }
        
        return count
    }
}
```

### Kotlin

```kotlin
class Solution {
    fun countKDifference(nums: IntArray, k: Int): Int {
        val freq = mutableMapOf<Int, Int>()
        var count = 0
        
        for (num in nums) {
            count += freq.getOrDefault(num + k, 0)
            count += freq.getOrDefault(num - k, 0)
            freq[num] = freq.getOrDefault(num, 0) + 1
        }
        
        return count
    }
}
```

### Dart

```dart
class Solution {
  int countKDifference(List<int> nums, int k) {
    final Map<int, int> freq = {};
    int count = 0;
    
    for (var num in nums) {
      count += (freq[num + k] ?? 0);
      count += (freq[num - k] ?? 0);
      freq[num] = (freq[num] ?? 0) + 1;
    }
    
    return count;
  }
}
```

### Go

```go
func countKDifference(nums []int, k int) int {
    freq := make(map[int]int)
    count := 0
    
    for _, num := range nums {
        count += freq[num + k]
        count += freq[num - k]
        freq[num]++
    }
    
    return count
}
```

### Ruby

```ruby
# @param {Integer[]} nums
# @param {Integer} k
# @return {Integer}
def count_k_difference(nums, k)
    freq = Hash.new(0)
    count = 0
    
    nums.each do |num|
        count += freq[num + k]
        count += freq[num - k]
        freq[num] += 1
    end
    
    return count
end
```

### Scala

```scala
object Solution {
    def countKDifference(nums: Array[Int], k: Int): Int = {
        val freq = scala.collection.mutable.Map[Int, Int]().withDefaultValue(0)
        var count = 0
        
        nums.foreach { num =>
            count += freq(num + k)
            count += freq(num - k)
            freq(num) += 1
        }
        
        count
    }
}
```

### Rust

```rust
impl Solution {
    pub fn count_k_difference(nums: Vec<i32>, k: i32) -> i32 {
        use std::collections::HashMap;
        
        let mut freq = HashMap::new();
        let mut count = 0;
        
        for &num in &nums {
            count += *freq.get(&(num + k)).unwrap_or(&0);
            count += *freq.get(&(num - k)).unwrap_or(&0);
            *freq.entry(num).or_insert(0) += 1;
        }
        
        count
    }
}
```

### Racket

```racket
(define/contract (count-k-difference nums k)
  (-> (listof exact-integer?) exact-integer? exact-integer?)
  (define freq (make-hash))
  (define count 0)
  (for ([num nums])
    (set! count (+ count (hash-ref freq (+ num k) 0)))
    (set! count (+ count (hash-ref freq (- num k) 0)))
    (hash-set! freq num (+ 1 (hash-ref freq num 0))))
  count)
```

### Erlang

```erlang
-spec count_k_difference(Nums :: [integer()], K :: integer()) -> integer().
count_k_difference(Nums, K) ->
    Freq = maps:new(),
    Count = 0,
    lists:foldl(fun(Num, {F, C}) ->
        {[NewF, NewC] || 
            NewC = C + maps:get(default, Num + K, 0, F), 
            NewC = NewC + maps:get(default, Num - K, 0, F),
            NewF = maps:update_with(Num, fun(X) -> X + 1 end, 1, F)};
        {NewF, NewC}
    end, {Freq, Count}, Nums).
```

### Elixir

```elixir
defmodule Solution do
  @spec count_k_difference(nums :: [integer], k :: integer) :: integer
  def count_k_difference(nums, k) do
    freq = %{}
    Enum.reduce(nums, 0, fn num, count ->
      count
      |> Kernel.+(Map.get(freq, num + k, 0))
      |> Kernel.+(Map.get(freq, num - k, 0))
      |> Kernel.+(Map.update(freq, num, 1, &(&1 + 1)))
    end)
  end
end
```

These code snippets efficiently solve the problem with a time complexity of \(O(n)\) and space complexity of \(O(n)\), where \(n\) is the length of the array.


### Closing Statement

Great job! We have thoroughly discussed the problem and implemented an optimized solution using a hashmap to reduce the time complexity from \(O(n^2)\) to \(O(n)\). This approach leverages the power of hashing to efficiently count pairs with an absolute difference equal to \(k\).

We also translated this solution across various programming languages, highlighting the importance of understanding algorithmic optimization and its implementation in different contexts. Each implementation maintains the same logic and efficiency, ensuring that the solution is both robust and scalable.

With this optimized approach, you can efficiently solve the problem while keeping the time and space complexities manageable, even for larger input sizes within the given constraints.

### Similar Questions

Here are some similar questions that you might find interesting and are good practice for understanding and applying hashmap-based techniques for array manipulation problems:

1. **Two Sum**: Given an array of integers `nums` and an integer `target`, return the indices of the two numbers such that they add up to `target`. (Uses hashmap for efficient lookup)
   - [LeetCode Problem #1](https://leetcode.com/problems/two-sum/)

2. **Subarray Sum Equals K**: Given an array of integers `nums` and an integer `k`, find the total number of continuous subarrays whose sum equals to `k`. (Uses hashmap to store cumulative sums)
   - [LeetCode Problem #560](https://leetcode.com/problems/subarray-sum-equals-k/)

3. **Longest Substring Without Repeating Characters**: Given a string `s`, find the length of the longest substring without repeating characters. (Uses hashmap to track characters and their positions)
   - [LeetCode Problem #3](https://leetcode.com/problems/longest-substring-without-repeating-characters/)

4. **Four Sum II**: Given four lists of integers, `A`, `B`, `C`, `D`, find all quadruplets `(a, b, c, d)` such that `A[a] + B[b] + C[c] + D[d] == 0`. (Uses hashing to store pairs of sums)
   - [LeetCode Problem #454](https://leetcode.com/problems/4sum-ii/)

5. **Pairs of Songs With Total Durations Divisible by 60**: Given a list of song durations, find the number of pairs of songs where the total duration of each pair is divisible by 60. (Uses hashmap to track remainders)
   - [LeetCode Problem #1010](https://leetcode.com/problems/pairs-of-songs-with-total-durations-divisible-by-60/)

These problems will help reinforce your understanding of hashmaps and how they can be effectively used to enhance the efficiency of various array and string manipulation algorithms. Keep practicing and exploring different algorithmic strategies!