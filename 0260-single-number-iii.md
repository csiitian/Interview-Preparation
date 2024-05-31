### Interviewer and Interviewee Discussion

**Interviewer:** Let's discuss a problem where you’re given an array of integers where exactly two elements appear only once and all other elements appear exactly twice. You need to find the two elements that appear only once. Does that sound clear?

**Interviewee:** Yes, that makes sense. So, essentially we have an array `nums` and among all its elements, only two appear exactly once while the rest appear twice. We need to find and return these two unique elements.

**Interviewer:** Correct. Let's first discuss a brute-force approach to solve this problem. How would you tackle it?

**Interviewee:** For a brute-force solution, a common idea that comes to mind is using a hash map or a dictionary to keep track of the occurrences of each element. By doing so, we can iterate through the array, update the count for each element, and then iterate through the hash map to extract the elements with a count of one.

**Interviewer:** Okay, let's walk through the brute-force approach step-by-step.

### Brute Force Approach

1. **Iterate through `nums`:** For each element, update its count in the hash map.
2. **Filter Results:** Once the hash map is built, iterate through it to find the elements with a count of one.

Here’s a sample implementation in Python:

```python
def singleNumber(nums):
    count = {}
    for num in nums:
        if num in count:
            count[num] += 1
        else:
            count[num] = 1
    
    result = []
    for num, cnt in count.items():
        if cnt == 1:
            result.append(num)
    
    return result
```

**Interviewer:** That works, but let’s discuss the time and space complexity of this approach.

**Interviewee:** Sure. 

- **Time Complexity:** Both the iterations, one through the array and one through the hash map, are linear in terms of time. Therefore, the total time complexity is O(n), where `n` is the length of the array.
- **Space Complexity:** The space complexity is O(n) as we need to store all the elements in the hash map, which in the worst case, is `n/2 + 2` elements (since all elements appear twice except two).

**Interviewer:** That’s correct. Now, let's try to optimize this solution for space, as the problem requires us to use constant extra space.

**Interviewee:** To achieve constant space, we can make use of bit manipulation, specifically the XOR operation. The XOR operation has some useful properties:
1. \( a ^ a = 0 \)
2. \( a ^ 0 = a \)
3. \( a ^ b ^ a = b \)

By XORing all elements of the array, the pairs of numbers will cancel out to zero, and we will be left with XOR of the two unique numbers.

**Interviewer:** Good point. How would you then separate these two unique numbers?

**Interviewee:** Once we have the combined XOR of the two unique numbers, say `x1 ^ x2`, we need to find a way to separate them. We can use any set bit in the result `x1 ^ x2` to distinguish between the two unique numbers. Let's choose the rightmost set bit (i.e., a bit that is `1`).

1. Compute `xor = x1 ^ x2`.
2. Find its rightmost set bit `set_bit = xor & -xor`.
3. Initialize two variables `a` and `b` to zero.
4. Iterate through the array and partition numbers into two groups based on the set bit.
5. XOR the numbers in each group, ending up with the two unique numbers.

Here's the implementation in Python:

```python
def singleNumber(nums):
    xor = 0
    for num in nums:
        xor ^= num
    
    set_bit = xor & -xor
    
    a, b = 0, 0
    for num in nums:
        if num & set_bit:
            a ^= num
        else:
            b ^= num
    
    return [a, b]
```

**Interviewer:** Let's discuss the time and space complexity of this optimized solution.

**Interviewee:** 

- **Time Complexity:** The approach still takes O(n) time because we are simply performing a linear scan of the array twice. First to compute the XOR and then to partition the numbers.
- **Space Complexity:** The space complexity is O(1) as we are using only a fixed number of extra variables, not any additional data structures.

**Interviewer:** Excellent! Would you like to draw it out for clarity?

**Interviewee:**

Sure, let's illustrate it visually.

```
Example: nums = [1, 2, 1, 3, 2, 5]

Step 1: Compute XOR of all elements: 
1 ^ 2 ^ 1 ^ 3 ^ 2 ^ 5 = 3 ^ 5 = 6 (In binary: 0110)

Step 2: Find rightmost set bit of 6:
6 & -6 = 2 (In binary: 0010)

Step 3: Partition numbers based on the rightmost set bit:
- Group 1 (bit set at 2nd position): [2, 2, 3]
- Group 2 (bit not set at 2nd position): [1, 1, 5]

Step 4: XOR numbers within each group:
- Group 1: 2 ^ 2 ^ 3 = 3
- Group 2: 1 ^ 1 ^ 5 = 5

Result: [3, 5]
```

**Interviewer:** Perfect. This clearly explains the optimized approach. Thank you for the detailed explanation and solution.
Sure, let's add the optimized solution for each language provided. The solution will use bit manipulation to ensure O(n) time complexity and O(1) space complexity.

### C++
```cpp
class Solution {
public:
    vector<int> singleNumber(vector<int>& nums) {
        int xorAll = 0;
        for (int num : nums) {
            xorAll ^= num;
        }

        int setBit = xorAll & -xorAll;
        int a = 0, b = 0;

        for (int num : nums) {
            if (num & setBit) {
                a ^= num;
            } else {
                b ^= num;
            }
        }

        return {a, b};
    }
};
```

### Java
```java
class Solution {
    public int[] singleNumber(int[] nums) {
        int xorAll = 0;
        for (int num : nums) {
            xorAll ^= num;
        }

        int setBit = xorAll & -xorAll;
        int a = 0, b = 0;
        
        for (int num : nums) {
            if ((num & setBit) != 0) {
                a ^= num;
            } else {
                b ^= num;
            }
        }

        return new int[]{a, b};
    }
}
```

### Python
#### Python 2
```python
class Solution(object):
    def singleNumber(self, nums):
        """
        :type nums: List[int]
        :rtype: List[int]
        """
        xor_all = 0
        for num in nums:
            xor_all ^= num

        set_bit = xor_all & -xor_all
        a, b = 0, 0

        for num in nums:
            if num & set_bit:
                a ^= num
            else:
                b ^= num

        return [a, b]
```

#### Python 3
```python
class Solution:
    def singleNumber(self, nums: List[int]) -> List[int]:
        xor_all = 0
        for num in nums:
            xor_all ^= num

        set_bit = xor_all & -xor_all
        a, b = 0, 0

        for num in nums:
            if num & set_bit:
                a ^= num
            else:
                b ^= num

        return [a, b]
```

### C
```c
/**
 * Note: The returned array must be malloced, assume caller calls free().
 */
int* singleNumber(int* nums, int numsSize, int* returnSize) {
    int xorAll = 0;
    for (int i = 0; i < numsSize; i++) {
        xorAll ^= nums[i];
    }

    int setBit = xorAll & -xorAll;
    int a = 0, b = 0;
    
    for (int i = 0; i < numsSize; i++) {
        if (nums[i] & setBit) {
            a ^= nums[i];
        } else {
            b ^= nums[i];
        }
    }

    int* result = (int*)malloc(2 * sizeof(int));
    result[0] = a;
    result[1] = b;
    *returnSize = 2;
    return result;
}
```

### C#
```csharp
public class Solution {
    public int[] SingleNumber(int[] nums) {
        int xorAll = 0;
        foreach (var num in nums) {
            xorAll ^= num;
        }

        int setBit = xorAll & -xorAll;
        int a = 0, b = 0;
        
        foreach (var num in nums) {
            if ((num & setBit) != 0) {
                a ^= num;
            } else {
                b ^= num;
            }
        }

        return new int[]{a, b};
    }
}
```

### JavaScript
```javascript
/**
 * @param {number[]} nums
 * @return {number[]}
 */
var singleNumber = function(nums) {
    let xorAll = 0;
    for (let num of nums) {
        xorAll ^= num;
    }

    let setBit = xorAll & -xorAll;
    let a = 0, b = 0;

    for (let num of nums) {
        if (num & setBit) {
            a ^= num;
        } else {
            b ^= num;
        }
    }

    return [a, b];
};
```

### TypeScript
```typescript
function singleNumber(nums: number[]): number[] {
    let xorAll = 0;
    for (let num of nums) {
        xorAll ^= num;
    }

    let setBit = xorAll & -xorAll;
    let a = 0, b = 0;

    for (let num of nums) {
        if (num & setBit) {
            a ^= num;
        } else {
            b ^= num;
        }
    }

    return [a, b];
};
```

### PHP
```php
class Solution {

    /**
     * @param Integer[] $nums
     * @return Integer[]
     */
    function singleNumber($nums) {
        $xorAll = 0;
        foreach ($nums as $num) {
            $xorAll ^= $num;
        }

        $setBit = $xorAll & -$xorAll;
        $a = 0;
        $b = 0;

        foreach ($nums as $num) {
            if ($num & $setBit) {
                $a ^= $num;
            } else {
                $b ^= $num;
            }
        }

        return [$a, $b];
    }
}
```

### Swift
```swift
class Solution {
    func singleNumber(_ nums: [Int]) -> [Int] {
        var xorAll = 0
        for num in nums {
            xorAll ^= num
        }

        let setBit = xorAll & -xorAll
        var a = 0
        var b = 0

        for num in nums {
            if num & setBit != 0 {
                a ^= num
            } else {
                b ^= num
            }
        }

        return [a, b]
    }
}
```

### Kotlin
```kotlin
class Solution {
    fun singleNumber(nums: IntArray): IntArray {
        var xorAll = 0
        for (num in nums) {
            xorAll ^= num
        }

        val setBit = xorAll and -xorAll
        var a = 0
        var b = 0

        for (num in nums) {
            if (num and setBit != 0) {
                a ^= num
            } else {
                b ^= num
            }
        }

        return intArrayOf(a, b)
    }
}
```

### Dart
```dart
class Solution {
  List<int> singleNumber(List<int> nums) {
    int xorAll = 0;
    for (int num in nums) {
      xorAll ^= num;
    }

    int setBit = xorAll & -xorAll;
    int a = 0, b = 0;

    for (int num in nums) {
      if (num & setBit > 0) {
        a ^= num;
      } else {
        b ^= num;
      }
    }

    return [a, b];
  }
}
```

### Go
```go
func singleNumber(nums []int) []int {
    xorAll := 0
    for _, num := range nums {
        xorAll ^= num
    }

    setBit := xorAll & -xorAll
    a, b := 0, 0

    for _, num := range nums {
        if num & setBit != 0 {
            a ^= num
        } else {
            b ^= num
        }
    }

    return []int{a, b}
}
```

### Ruby
```ruby
# @param {Integer[]} nums
# @return {Integer[]}
def single_number(nums)
    xor_all = 0
    nums.each do |num|
        xor_all ^= num
    end

    set_bit = xor_all & -xor_all
    a, b = 0, 0

    nums.each do |num|
        if num & set_bit != 0
            a ^= num
        else
            b ^= num
        end
    end

    [a, b]
end
```

### Scala
```scala
object Solution {
    def singleNumber(nums: Array[Int]): Array[Int] = {
        var xorAll = 0
        for (num <- nums) {
            xorAll ^= num
        }

        val setBit = xorAll & -xorAll
        var a = 0
        var b = 0

        for (num <- nums) {
            if ((num & setBit) != 0) {
                a ^= num
            } else {
                b ^= num
            }
        }

        Array(a, b)
    }
}
```

### Rust
```rust
impl Solution {
    pub fn single_number(nums: Vec<i32>) -> Vec<i32> {
        let mut xor_all = 0;
        for &num in &nums {
            xor_all ^= num;
        }

        let set_bit = xor_all & !xor_all + 1;
        let (mut a, mut b) = (0, 0);

        for &num in &nums {
            if num & set_bit != 0 {
                a ^= num;
            } else {
                b ^= num;
            }
        }

        vec![a, b]
    }
}
```

### Racket
```racket
(define/contract (single-number nums)
  (-> (listof exact-integer?) (listof exact-integer?))
  (define xor-all
    (foldl bitwise-xor 0 nums))
  (define set-bit
    (bitwise-and xor-all (- xor-all)))
  (define (split (num val))
    (if (zero? (bitwise-and num set-bit))
        (values (bitwise-xor val num) b)
        (values a (bitwise-xor b num))))
  (define-values [a b]
    (foldl split 0 0 nums))
  (list a b))
```

### Erlang
```erlang
-spec single_number(Nums :: [integer()]) -> [integer()].
single_number(Nums) ->
    XorAll = lists:foldl(fun erlang:'xor'/2, 0, Nums),
    SetBit = XorAll band bnot(XorAll - 1),
    {A, B} = lists:foldl(
        fun(Num, {A1, B1}) ->
            if
                Num band SetBit =/= 0 -> {A1 bxor Num, B1};
                true -> {A1, B1 bxor Num}
            end
        end,
        {0, 0},
        Nums
    ),
    [A, B].
```

### Elixir
```elixir
defmodule Solution do
  @spec single_number(nums :: [integer]) :: [integer]
  def single_number(nums) do
    xor_all = Enum.reduce(nums, 0, &Kernel.bitwise_xor/2)
    set_bit = xor_all &&& -xor_all

    {a, b} = Enum.reduce(nums, {0, 0}, fn num, {a, b} ->
      if (num &&& set_bit) != 0 do
        {a ^^^ num, b}
      else
        {a, b ^^^ num}
      end
    end)

    [a, b]
  end
end
```
### Closing Statement

Thank you for discussing this problem today. We started by outlining a brute-force approach using a hash map and discussed its time and space complexity. Then, we optimized the solution using bit manipulation to ensure O(n) time complexity and O(1) space complexity. This efficient approach leverages the properties of the XOR operation to identify the two unique elements in a linear pass while using a constant amount of extra space. We also provided implementations for multiple programming languages to illustrate this solution comprehensively.

This problem demonstrates how understanding and leveraging bitwise operations can significantly optimize the performance of your solutions in terms of both time and space complexity. Such optimizations are often crucial in scenarios involving large datasets or performance-critical applications.

### Similar Questions

1. **Single Number**: Given a non-empty array of integers, all of which appear twice except for one. Find that single one. [Link to problem](https://leetcode.com/problems/single-number/)
2. **Single Number III**: Given an array of integers, every element appears three times except for one. Find that single one. [Link to problem](https://leetcode.com/problems/single-number-ii/)
3. **Missing Number**: Given an array containing `n` distinct numbers taken from the range `0` to `n`. Find the one that is missing from the array. [Link to problem](https://leetcode.com/problems/missing-number/)
4. **Find the Duplicate Number**: Given an array of integers containing `n + 1` integers where each integer is between `1` and `n` inclusive. There is only one duplicate number. Find that duplicate number. [Link to problem](https://leetcode.com/problems/find-the-duplicate-number/)
5. **Two Sum**: Given an array of integers, return indices of the two numbers such that they add up to a specific target. [Link to problem](https://leetcode.com/problems/two-sum/)

By practicing these related problems, you will further enhance your problem-solving skills and understanding of the application of various algorithmic techniques, particularly in the context of bitwise operations and hash maps.

Thank you, and happy coding!