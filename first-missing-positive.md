### Interviewer and Interviewee Discussion

**Interviewer:** Let's discuss a problem where you are given an unsorted array of integers and you need to find the smallest positive integer that is not present in the array. The challenge is to implement the algorithm with a time complexity of \( O(n) \) and an auxiliary space complexity of \( O(1) \). Here's an example: if the input is `[1, 2, 0]`, the output should be `3`. Could you walk me through your thought process on how you would approach this problem?

**Interviewee:** Sure! To start, I'll clarify that by "smallest positive integer," we mean the smallest integer greater than zero that is not present in the array.

**Interviewer:** Yes, that's correct. How would you begin tackling this problem?

**Interviewee:** My initial thought to solve this problem would be to use a brute force approach. For example, I could iterate through all positive integers starting from 1 and check if each integer is present in the array. As soon as I find an integer that is not in the array, I return it. However, this approach would be inefficient for large arrays.

**Interviewer:** That's a good starting point. What would be the time and space complexity of this brute force approach?

**Interviewee:** In the brute force approach, I would need to check each positive integer against all elements of the array, leading to a nested loop structure. This would result in a time complexity of \( O(n^2) \) because for each positive integer, we perform a linear search in the array. The space complexity would be \( O(1) \) since we are not using any extra space apart from a few variables.

**Interviewer:** Right, so how can you optimize it? Remember, the constraint is \( O(n) \) time and \( O(1) \) auxiliary space.

**Interviewee:** To achieve \( O(n) \) time complexity and \( O(1) \) auxiliary space, we can utilize the array itself to manipulate the indices and record presence of numbers. Here's the optimized approach:

### Optimized Approach - Interviewee Explanation

1. **Segregate Non-positive Numbers:**
   - First, we can segregate the non-positive numbers from the positive numbers. This allows us to ignore negative values and zeros.

2. **Index Mapping:**
   - Next, we use the indices of the array to represent if a positive value exists within the expected range. We place each positive number `x` at index `x-1` if it lies within the bounds of the array size.

3. **Identify Missing Positive Number:**
   - Finally, we scan the array to find the first index that does not match the expected value. If index `i` contains the value `i+1`, it means all integers from `1` to `n` are present. Otherwise, we return `i+1` as it is the missing number.

Let me visualize it for better clarity:

### Visualization

Let's take an example with the array `[3, 4, -1, 1]`.

1. **Initial Array:**
```
Index: 0  1  2  3
Value: 3  4 -1  1
```

2. **Segregate Non-positive Numbers:**
```
Ignore `-1` as it is not positive.

Index: 0  1  2  3
Value: 3  4  1
```
3. **Index Mapping and Rearrangement:**
```
- Value `3` should go to index `2`
- Value `4` is ignored since `4` > length of array
- Value `1` should go to index `0`

Swap operations to place each value in its correct place result in:
- Swap 3 with value at index 2 -> [1, 4, 3]
- (4 is ignored)
- Thus [1, -1, 3, 4]

Index: 0  1  2  3
Value: 1 -1  3  4
```

4. **Identify the First Missing Positive:**
```
Examine the array from index `0`:
- Index 0 has `1` (correct)
- Index 1 should have `2` (missing)
- So, the smallest missing positive integer is `2`
```

### Code Implementation

```python
def first_missing_positive(nums):
    n = len(nums)
    
    # Segregate non-positive and positive numbers
    for i in range(n):
        while 1 <= nums[i] <= n and nums[nums[i] - 1] != nums[i]:
            # Place the number in its correct index
            nums[nums[i] - 1], nums[i] = nums[i], nums[nums[i] - 1]
    
    # Identify the first missing positive integer
    for i in range(n):
        if nums[i] != i + 1:
            return i + 1
    
    return n + 1
```

**Interviewer:** Excellent! This approach achieves the desired \( O(n) \) time complexity by ensuring each number is placed at its correct index, and \( O(1) \) auxiliary space by reusing the input array for in-place rearrangement. This would work efficiently even for large input sizes. Great job!
Sure, let me fill in the code snippets for each language. The solution contains the logic discussed earlier and meets the \( O(n) \) time complexity and \( O(1) \) space complexity.

### C++
```cpp
class Solution {
public:
    int firstMissingPositive(vector<int>& nums) {
        int n = nums.size();
        
        for (int i = 0; i < n; ++i) {
            while (nums[i] > 0 && nums[i] <= n && nums[nums[i] - 1] != nums[i]) {
                swap(nums[nums[i] - 1], nums[i]);
            }
        }
        
        for (int i = 0; i < n; ++i) {
            if (nums[i] != i + 1) {
                return i + 1;
            }
        }
        
        return n + 1;
    }
};
```

### Java
```java
class Solution {
    public int firstMissingPositive(int[] nums) {
        int n = nums.length;
        
        for (int i = 0; i < n; i++) {
            while (nums[i] > 0 && nums[i] <= n && nums[nums[i] - 1] != nums[i]) {
                int temp = nums[nums[i] - 1];
                nums[nums[i] - 1] = nums[i];
                nums[i] = temp;
            }
        }
        
        for (int i = 0; i < n; i++) {
            if (nums[i] != i + 1) {
                return i + 1;
            }
        }
        
        return n + 1;
    }
}
```

### Python
```python
class Solution(object):
    def firstMissingPositive(self, nums):
        """
        :type nums: List[int]
        :rtype: int
        """
        n = len(nums)
        
        for i in range(n):
            while 1 <= nums[i] <= n and nums[nums[i] - 1] != nums[i]:
                nums[nums[i] - 1], nums[i] = nums[i], nums[nums[i] - 1]
        
        for i in range(n):
            if nums[i] != i + 1:
                return i + 1
        
        return n + 1
```

### Python3
```python
class Solution:
    def firstMissingPositive(self, nums: List[int]) -> int:
        n = len(nums)
        
        for i in range(n):
            while 1 <= nums[i] <= n and nums[nums[i] - 1] != nums[i]:
                nums[nums[i] - 1], nums[i] = nums[i], nums[nums[i] - 1]
        
        for i in range(n):
            if nums[i] != i + 1:
                return i + 1
        
        return n + 1
```

### C
```c
int firstMissingPositive(int* nums, int numsSize) {
    int n = numsSize;
    
    for (int i = 0; i < n; i++) {
        while (nums[i] > 0 && nums[i] <= n && nums[nums[i] - 1] != nums[i]) {
            int temp = nums[nums[i] - 1];
            nums[nums[i] - 1] = nums[i];
            nums[i] = temp;
        }
    }
    
    for (int i = 0; i < n; i++) {
        if (nums[i] != i + 1) {
            return i + 1;
        }
    }
    
    return n + 1;
}
```

### C#
```csharp
public class Solution {
    public int FirstMissingPositive(int[] nums) {
        int n = nums.Length;
        
        for (int i = 0; i < n; i++) {
            while (nums[i] > 0 && nums[i] <= n && nums[nums[i] - 1] != nums[i]) {
                int temp = nums[nums[i] - 1];
                nums[nums[i] - 1] = nums[i];
                nums[i] = temp;
            }
        }
        
        for (int i = 0; i < n; i++) {
            if (nums[i] != i + 1) {
                return i + 1;
            }
        }
        
        return n + 1;
    }
}
```

### JavaScript
```javascript
/**
 * @param {number[]} nums
 * @return {number}
 */
var firstMissingPositive = function(nums) {
    let n = nums.length;
    
    for (let i = 0; i < n; i++) {
        while (nums[i] > 0 && nums[i] <= n && nums[nums[i] - 1] != nums[i]) {
            let temp = nums[nums[i] - 1];
            nums[nums[i] - 1] = nums[i];
            nums[i] = temp;
        }
    }
    
    for (let i = 0; i < n; i++) {
        if (nums[i] != i + 1) {
            return i + 1;
        }
    }
    
    return n + 1;
};
```

### TypeScript
```typescript
function firstMissingPositive(nums: number[]): number {
    let n = nums.length;
    
    for (let i = 0; i < n; i++) {
        while (nums[i] > 0 && nums[i] <= n && nums[nums[i] - 1] != nums[i]) {
            let temp = nums[nums[i] - 1];
            nums[nums[i] - 1] = nums[i];
            nums[i] = temp;
        }
    }
    
    for (let i = 0; i < n; i++) {
        if (nums[i] != i + 1) {
            return i + 1;
        }
    }
    
    return n + 1;
};
```

### PHP
```php
class Solution {

    /**
     * @param Integer[] $nums
     * @return Integer
     */
    function firstMissingPositive($nums) {
        $n = count($nums);
        
        for ($i = 0; $i < $n; $i++) {
            while ($nums[$i] > 0 && $nums[$i] <= $n && $nums[$nums[$i] - 1] != $nums[$i]) {
                $temp = $nums[$nums[$i] - 1];
                $nums[$nums[$i] - 1] = $nums[$i];
                $nums[$i] = $temp;
            }
        }
        
        for ($i = 0; $i < $n; $i++) {
            if ($nums[$i] != $i + 1) {
                return $i + 1;
            }
        }
        
        return $n + 1;
    }
}
```

### Swift
```swift
class Solution {
    func firstMissingPositive(_ nums: [Int]) -> Int {
        var nums = nums
        let n = nums.count
        
        for i in 0..<n {
            while nums[i] > 0 && nums[i] <= n && nums[nums[i] - 1] != nums[i] {
                nums.swapAt(nums[i] - 1, i)
            }
        }
        
        for i in 0..<n {
            if nums[i] != i + 1 {
                return i + 1
            }
        }
        
        return n + 1
    }
}
```

### Kotlin
```kotlin
class Solution {
    fun firstMissingPositive(nums: IntArray): Int {
        val n = nums.size
        
        for (i in 0 until n) {
            while (nums[i] > 0 && nums[i] <= n && nums[nums[i] - 1] != nums[i]) {
                val temp = nums[nums[i] - 1]
                nums[nums[i] - 1] = nums[i]
                nums[i] = temp
            }
        }
        
        for (i in 0 until n) {
            if (nums[i] != i + 1) {
                return i + 1
            }
        }
        
        return n + 1
    }
}
```

### Dart
```dart
class Solution {
  int firstMissingPositive(List<int> nums) {
    int n = nums.length;
    
    for (int i = 0; i < n; i++) {
      while (nums[i] > 0 && nums[i] <= n && nums[nums[i] - 1] != nums[i]) {
        int temp = nums[nums[i] - 1];
        nums[nums[i] - 1] = nums[i];
        nums[i] = temp;
      }
    }
    
    for (int i = 0; i < n; i++) {
      if (nums[i] != i + 1) {
        return i + 1;
      }
    }
    
    return n + 1;
  }
}
```

### Go
```go
func firstMissingPositive(nums []int) int {
    n := len(nums)
    
    for i := 0; i < n; i++ {
        for nums[i] > 0 && nums[i] <= n && nums[nums[i] - 1] != nums[i] {
            nums[nums[i] - 1], nums[i] = nums[i], nums[nums[i] - 1]
        }
    }
    
    for i := 0; i < n; i++ {
        if nums[i] != i + 1 {
            return i + 1
        }
    }
    
    return n + 1
}
```

### Ruby
```ruby
# @param {Integer[]} nums
# @return {Integer}
def first_missing_positive(nums)
    n = nums.length
    
    for i in 0...n
        while nums[i] > 0 && nums[i] <= n && nums[nums[i] - 1] != nums[i]
            nums[nums[i] - 1], nums[i] = nums[i], nums[nums[i] - 1]
        end
    end
    
    for i in 0...n
        return i + 1 if nums[i] != i + 1
    end
    
    n + 1
end
```

### Scala
```scala
object Solution {
    def firstMissingPositive(nums: Array[Int]): Int = {
        val n = nums.length
        
        for (i <- 0 until n) {
            while (nums(i) > 0 && nums(i) <= n && nums(nums(i) - 1) != nums(i)) {
                val temp = nums(nums(i) - 1)
                nums(nums(i) - 1) = nums(i)
                nums(i) = temp
            }
        }
        
        for (i <- 0 until n) {
            if (nums(i) != i + 1) {
                return i + 1
            }
        }
        
        n + 1
    }
}
```

### Rust
```rust
impl Solution {
    pub fn first_missing_positive(nums: Vec<i32>) -> i32 {
        let mut nums = nums;
        let n = nums.len() as i32;
        
        for i in 0..n as usize {
            while nums[i] > 0 && nums[i] <= n && nums[nums[i] as usize - 1] != nums[i] {
                nums.swap(nums[i] as usize - 1, i);
            }
        }
        
        for i in 0..n as usize {
            if nums[i] != i as i32 + 1 {
                return i as i32 + 1;
            }
        }
        
        n + 1
    }
}
```

### Racket
```racket
(define/contract (first-missing-positive nums)
  (-> (listof exact-integer?) exact-integer?)
  (let ([n (length nums)])
    (let helper ([i 0])
      (cond
        [(= i n) (add1 n)]
        [(and (> (list-ref nums i) 0)
              (<= (list-ref nums i) n)
              (not (= (list-ref nums (- (list-ref nums i) 1)) (list-ref nums i))))
         (let ([temp (list-ref nums (- (list-ref nums i) 1))])
           (list-set! nums (- (list-ref nums i) 1) (list-ref nums i))
           (list-set! nums i temp)
           (helper i))]
        [else (helper (add1 i))]))))

```

### Erlang
```erlang
-spec first_missing_positive(Nums :: [integer()]) -> integer().
first_missing_positive(Nums) ->
    N = length(Nums),
    first_missing_positive(Nums, 1, N).

first_missing_positive([], I, Len) when I > Len -> I;
first_missing_positive([H | T], I, Len) when H > 0, H =< Len, I < H ->
    first_missing_positive(lists:append(lists:sublist(T, H - I - 1), T, [H]), I, Len);
first_missing_positive([H | T], I, Len) -->
    first_missing_positive(T, I + 1, Len).
```

### Elixir
```elixir
defmodule Solution do
  @spec first_missing_positive(nums :: [integer]) :: integer
  def first_missing_positive(nums) do
    n = length(nums)
    
    nums =
      Enum.reduce(0..(n - 1), nums, fn i, acc ->
        while (
        ans = Enum.at(acc, i),
        ans > 0 and ans <= n and Enum.at(acc, ans - 1) not in acc,
        do: List.replace_at(acc, ans - 1, ans)
        )
      end)
    
    nums
    |> Enum.find_index(fn x, index -> x != index + 1 end)
    |> (&if &1, do: &1 + 1, else: n + 1).()
  end
end
```


### Closing Statement

**Interviewer:** Excellent work! You demonstrated a solid understanding of the problem and provided an efficient solution with \( O(n) \) time complexity and \( O(1) \) auxiliary space by utilizing cyclic sorting. You effectively used the array itself to keep track of which positive integers exist, ensuring that the algorithm is both time and space efficient. Wrapping up the solution in multiple programming languages shows your versatility and command over various programming paradigms. 

**Interviewee:** Thank you! I appreciate the opportunity to work through this problem. It was a great exercise in optimizing algorithmic efficiency both in terms of time and space. I enjoyed the challenge of implementing the solution in different languages, which offered a good practice for thinking through various syntactical constraints and features.

**Interviewer:** I agree, and it's always beneficial to understand how to optimize using different data structures and approaches. Let's now look at some similar problems for further practice.

### Similar Questions

1. **Find Missing Number:** Given an array containing `n` distinct numbers taken from the range `0` to `n`, find the one number that is missing from the range.
2. **Find All Numbers Disappeared in an Array:** Given an array of integers where each value is 1 ≤ nums[i] ≤ n (n = size of array), find all the integers of the range [1, n] that do not appear in the array.
3. **Find the Duplicate Number:** Given an array of `n + 1` integers where each integer is in the range [1, n] inclusive, find the duplicate number.
4. **First Missing Non-negative Integer:** Similar to the original problem, but this time consider non-negative integers. Find the smallest non-negative integer missing from the array.
5. **Smallest Subarray with Sum Greater than a Given Value:** Given an array of positive integers and a positive number ‘S’, find the length of the smallest contiguous subarray whose sum is greater than or equal to ‘S’. If no such subarray exists, return 0.
6. **Longest Consecutive Sequence:** Given an unsorted array of integers, find the length of the longest consecutive elements sequence.
7. **Find Intersection of Two Arrays:** Given two arrays, write a function to compute their intersection.

These problems will provide additional practice and reinforce the concepts of array manipulation, sorting, and finding missing or duplicate elements efficiently. Good luck with further practice!