### Interviewer and Interviewee Discussion

**Interviewer:** Let's discuss a problem where you have an integer array `nums` where every element appears three times except for one, which appears exactly once. Your task is to find this unique element and return it. The solution must have linear runtime complexity and use only constant extra space. How would you approach this problem?

**Interviewee:** Okay, I understand the problem. One straightforward approach could be to use a hash map to keep track of the frequency of each element, but that would require extra space for the hash map. Since we need a solution with constant space, we need to think of a different way.

**Interviewer:** That's a good starting point. Can you discuss how a brute force approach might look like?

**Interviewee:** Sure. A brute force approach would involve checking the frequency of each element. We could use two nested loops to count the occurrences of each element. So, for each element, we would loop through the array to count how many times that element appears. This approach would have a time complexity of \(O(n^2)\) and a space complexity of \(O(1)\), which isn't optimal according to the problem constraints.

### Brute Force Approach

- **Time Complexity:** \(O(n^2)\)
- **Space Complexity:** \(O(1)\)

**Interviewer:** You're right, the brute force approach is not efficient. Can you think of a way to optimize it?

**Interviewee:** We need to keep track of the counts of bits across the array without extra space. One efficient way is to use bitwise operators. We can represent each bit position with two variables and update these variables for each number in the array.

### Optimized Approach Using Bitwise Operations

**Interviewee:** We can use two variables `ones` and `twos` to achieve the required result. The idea is to store the bits that appear once and twice, respectively. Here's how it can be done:

1. Initialize `ones` and `twos` to 0.
2. For each number in the array, update `ones` and `twos` based on the current number.
3. Finally, `ones` will hold the unique number that appears exactly once.

Here's the detailed breakdown:
- `ones ^= number`: Adds the bits of `number` to `ones` if they aren't already in `twos`.
- `twos |= ones & number`: Adds the bits of `number` to `twos` if they are currently in `ones`.
- `mask = ~(ones & twos)`: Creates a mask to remove bits that appeared three times.
- `ones &= mask`: Removes the bits that appeared three times from `ones`.
- `twos &= mask`: Removes the bits that appeared three times from `twos`.

### Time and Space Complexity

- **Time Complexity:** \(O(n)\) - We iterate through the array once.
- **Space Complexity:** \(O(1)\) - We only use a few extra variables.

### Optimized Code

```python
def singleNumber(nums):
    ones, twos = 0, 0
    for num in nums:
        ones = (ones ^ num) & ~twos
        twos = (twos ^ num) & ~ones
    return ones
```

### Diagram for Bitwise Operation

Imagine the number `num` has bit values like this: `num = 0011`. We will use `ones` and `twos` variables to track bit counts. Here’s a step-by-step diagram:

- **Initial State:**
  - `ones = 0000`
  - `twos = 0000`

- **After Processing first 0011:**
  - `ones = (0000 ^ 0011) & ~(0000) = 0011`
  - `twos = (0000 ^ 0011) & ~(0011) = 0000`

- **After Processing second 0011:**
  - `ones = (0011 ^ 0011) & ~(0000) = 0000`
  - `twos = (0000 ^ 0011) & ~(0000) = 0011`

- **After Processing third 0011:**
  - `ones = (0000 ^ 0011) & ~(0011) = 0000`
  - `twos = (0011 ^ 0011) & ~(0000) = 0000`

The bits in `ones` and `twos` that have appeared three times will be cleared. When the unique number appears, it will remain in `ones`.

**Interviewer:** Excellent! This bitwise approach is both efficient and meets the problem constraints. Thank you for the detailed explanation and solution.
### C++

```cpp
class Solution {
public:
    int singleNumber(vector<int>& nums) {
        int ones = 0, twos = 0;
        for (int num : nums) {
            ones = (ones ^ num) & ~twos;
            twos = (twos ^ num) & ~ones;
        }
        return ones;
    }
};

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Java

```java
class Solution {
    public int singleNumber(int[] nums) {
        int ones = 0, twos = 0;
        for (int num : nums) {
            ones = (ones ^ num) & ~twos;
            twos = (twos ^ num) & ~ones;
        }
        return ones;
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Python

```python
class Solution(object):
    def singleNumber(self, nums):
        """
        :type nums: List[int]
        :rtype: int
        """
        ones, twos = 0, 0
        for num in nums:
            ones = (ones ^ num) & ~twos
            twos = (twos ^ num) & ~ones
        return ones

# Time Complexity: O(n)
# Space Complexity: O(1)
```

### Python3

```python
class Solution:
    def singleNumber(self, nums: List[int]) -> int:
        ones, twos = 0, 0
        for num in nums:
            ones = (ones ^ num) & ~twos
            twos = (twos ^ num) & ~ones
        return ones

# Time Complexity: O(n)
# Space Complexity: O(1)
```

### C

```c
int singleNumber(int* nums, int numsSize) {
    int ones = 0, twos = 0;
    for (int i = 0; i < numsSize; i++) {
        ones = (ones ^ nums[i]) & ~twos;
        twos = (twos ^ nums[i]) & ~ones;
    }
    return ones;
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### C#

```csharp
public class Solution {
    public int SingleNumber(int[] nums) {
        int ones = 0, twos = 0;
        foreach (int num in nums) {
            ones = (ones ^ num) & ~twos;
            twos = (twos ^ num) & ~ones;
        }
        return ones;
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### JavaScript

```javascript
/**
 * @param {number[]} nums
 * @return {number}
 */
var singleNumber = function(nums) {
    let ones = 0, twos = 0;
    for (let num of nums) {
        ones = (ones ^ num) & ~twos;
        twos = (twos ^ num) & ~ones;
    }
    return ones;
};

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### TypeScript

```typescript
function singleNumber(nums: number[]): number {
    let ones = 0, twos = 0;
    for (let num of nums) {
        ones = (ones ^ num) & ~twos;
        twos = (twos ^ num) & ~ones;
    }
    return ones;
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### PHP

```php
class Solution {

    /**
     * @param Integer[] $nums
     * @return Integer
     */
    function singleNumber($nums) {
        $ones = 0; $twos = 0;
        foreach ($nums as $num) {
            $ones = ($ones ^ $num) & ~$twos;
            $twos = ($twos ^ $num) & ~$ones;
        }
        return $ones;
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Swift

```swift
class Solution {
    func singleNumber(_ nums: [Int]) -> Int {
        var ones = 0, twos = 0
        for num in nums {
            ones = (ones ^ num) & ~twos
            twos = (twos ^ num) & ~ones
        }
        return ones
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Kotlin

```kotlin
class Solution {
    fun singleNumber(nums: IntArray): Int {
        var ones = 0
        var twos = 0
        for (num in nums) {
            ones = (ones xor num) and twos.inv()
            twos = (twos xor num) and ones.inv()
        }
        return ones
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Dart

```dart
class Solution {
  int singleNumber(List<int> nums) {
    int ones = 0, twos = 0;
    for (int num in nums) {
        ones = (ones ^ num) & ~twos;
        twos = (twos ^ num) & ~ones;
    }
    return ones;
  }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Go

```go
func singleNumber(nums []int) int {
    ones, twos := 0, 0
    for _, num := range nums {
        ones = (ones ^ num) & ^twos
        twos = (twos ^ num) & ^ones
    }
    return ones
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Ruby

```ruby
# @param {Integer[]} nums
# @return {Integer}
def single_number(nums)
    ones, twos = 0, 0
    nums.each do |num|
        ones = (ones ^ num) & ~twos
        twos = (twos ^ num) & ~ones
    end
    ones
end

# Time Complexity: O(n)
# Space Complexity: O(1)
```

### Scala

```scala
object Solution {
    def singleNumber(nums: Array[Int]): Int = {
        var ones = 0
        var twos = 0
        for (num <- nums) {
            ones = (ones ^ num) & ~twos
            twos = (twos ^ num) & ~ones
        }
        ones
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Rust

```rust
impl Solution {
    pub fn single_number(nums: Vec<i32>) -> i32 {
        let mut ones = 0;
        let mut twos = 0;
        for num in nums {
            ones = (ones ^ num) & !twos;
            twos = (twos ^ num) & !ones;
        }
        ones
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Racket

```racket
(define/contract (single-number nums)
  (-> (listof exact-integer?) exact-integer?)
  (define ones 0)
  (define twos 0)
  (for-each
   (lambda (num)
     (set! ones (bitwise-and (bitwise-xor ones num) (bitwise-not twos)))
     (set! twos (bitwise-and (bitwise-xor twos num) (bitwise-not ones))))
   nums)
  ones)

;; Time Complexity: O(n)
;; Space Complexity: O(1)
```

### Erlang

```erlang
-spec single_number(Nums :: [integer()]) -> integer().
single_number(Nums) ->
    single_number(Nums, 0, 0).

single_number([], Ones, _) -> 
    Ones;
single_number([Num | Rest], Ones, Twos) ->
    NewOnes = (Ones bxor Num) band bnot Twos,
    NewTwos = (Twos bxor Num) band bnot NewOnes,
    single_number(Rest, NewOnes, NewTwos).

%% Time Complexity: O(n)
%% Space Complexity: O(1)
```

### Elixir

```elixir
defmodule Solution do
  @spec single_number(nums :: [integer]) :: integer
  def single_number(nums) do
    {ones, _twos} = Enum.reduce(nums, {0, 0}, fn num, {ones, twos} ->
      new_ones = (ones ^^^ num) &&& bnot(twos)
      new_twos = (twos ^^^ num) &&& bnot(new_ones)
      {new_ones, new_twos}
    end)
    ones
  end
end

# Time Complexity: O(n)
# Space Complexity: O(1)
```


### Closing Statement

**Interviewer:** Excellent job! You've successfully derived a highly efficient solution for the problem using bitwise operations, achieving the required linear runtime complexity and constant space complexity. This level of optimization demonstrates a strong understanding of bit manipulation and algorithm design. Your ability to articulate the problem, discuss potential approaches, and implement the optimized solution in various programming languages is commendable. Keep up the good work!

**Interviewee:** Thank you! This was a challenging but insightful problem. I appreciate the opportunity to demonstrate and enhance my problem-solving skills. Learning to leverage bitwise operations for such problems will definitely be useful in the future.

### Similar Questions

Here are some similar questions that you might find interesting and which also involve thinking about unique elements, bitwise manipulation, or linear time complexity:

1. **Single Number**:
   - **Description:** Given a non-empty array of integers, every element appears twice except for one. Find that single one.
   - **Constraints:** Implement the solution with a linear runtime complexity and without using extra memory.
   - **Example:** `nums = [2, 3, 2]; Output = 3`

2. **Single Number II**:
   - **Description:** Given a non-empty array of integers, every element appears four times except for one, which appears exactly once. Find that single one.
   - **Constraints:** Implement the solution with a linear runtime complexity and without using extra memory.
   - **Example:** `nums = [2, 2, 2, 2, 3]; Output = 3`

3. **Missing Number**:
   - **Description:** Given an array `nums` containing `n` distinct numbers in the range `[0, n]`, return the only number in the range that is missing from the array.
   - **Constraints:** Implement the solution with a linear runtime complexity.
   - **Example:** `nums = [3, 0, 1]; Output = 2`

4. **Find All Duplicates in an Array**:
   - **Description:** Given an array of integers, 1 ≤ a[i] ≤ n (n = size of array), some elements appear twice and others appear once. Find all the elements that appear twice.
   - **Constraints:** Implement the solution with a linear runtime complexity and without using extra memory.
   - **Example:** `nums = [4, 3, 2, 7, 8, 2, 3, 1]; Output = [2, 3]`

5. **Find the Duplicate Number**:
   - **Description:** Given an array of integers containing `n+1` integers where each integer is between 1 and `n` (inclusive), prove that at least one duplicate number must exist. Assume that there is only one duplicate number, find the duplicate one.
   - **Constraints:** Implement the solution with a linear runtime complexity and constant space complexity.
   - **Example:** `nums = [1, 3, 4, 2, 2]; Output = 2`

These questions will help you further hone your skills in handling arrays, bitwise operations, and optimizing algorithms for linear time complexity. Happy coding!