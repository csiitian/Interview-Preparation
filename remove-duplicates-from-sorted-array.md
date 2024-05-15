### Interviewer and Interviewee Discussion

**Interviewer:** Let's discuss a problem where you need to remove duplicates from a sorted array in place. Specifically, given an integer array `nums` sorted in non-decreasing order, your task is to remove the duplicates such that each unique element appears only once. The relative order of the elements should remain the same and you need to do this in place. You should return the number of unique elements in the array.

**Interviewee:** Got it. So, the output should be the number of unique elements, and the first `k` elements of the array should contain these unique elements. The rest of the array can be left as it is or filled with any values since they are not important.

**Interviewer:** Correct. Let’s discuss how you would approach this problem initially with a brute force method.

### Brute Force Approach

**Interviewee:** For the brute force approach, I could iterate through the array and use an auxiliary data structure, such as a list or set, to keep track of the unique elements we have encountered so far. Here’s how I can go about it:

1. Create an empty set to store unique elements.
2. Iterate through each element in the array:
   - If the element is not in the set, add it to the set and copy it to the position in the array where the next unique element should be stored.
3. Keep a counter to track the number of unique elements.

Here is a simple pseudo-code representation of the approach:

```
Set unique_elements = {}
Index idx = 0

For each element in nums:
    If element is not in unique_elements:
        Add element to unique_elements
        nums[idx] = element
        idx += 1

Return idx
```

**Interviewer:** That makes sense. Can you analyze the time and space complexity of this approach?

**Interviewee:** Sure. 

- **Time Complexity:** The time complexity is `O(n)` because we are traversing the entire array once, and set operations (checking for an element and adding an element) are on average `O(1)`.
- **Space Complexity:** The space complexity is `O(n)` because in the worst case, if there are no duplicates, we store all elements in the set.

### Optimized Approach

**Interviewer:** Good. Now, let's discuss optimizing this approach. Considering the constraints and the problem's requirements, is there a way to achieve the same result without using extra space?

**Interviewee:** Absolutely. Given that the array is already sorted, we can utilize this property to avoid using extra space by using a two-pointer approach.

**Interviewer:** Excellent! Explain how you'd implement this two-pointer approach.

### Optimized Two-Pointer Approach

**Interviewee:** Here’s how the two-pointer technique works:

1. Use two pointers, `i` and `k`.
2. `k` will point to the position where the next unique element should be placed.
3. `i` will traverse the entire array.
4. Initialize `k` to 1 because the first element is always unique.
5. For each element `nums[i]` starting from the second element:
   - If `nums[i]` is different from `nums[k-1]`, it means we have encountered a new unique element, so we place `nums[i]` at `nums[k]` and increment `k`.

Here's the actual code for the two-pointer approach:

```python
def removeDuplicates(nums):
    if not nums:
        return 0

    k = 1  # start with the second position because first element is always unique

    for i in range(1, len(nums)):
        if nums[i] != nums[i - 1]:  # compare with the previous element
            nums[k] = nums[i]
            k += 1

    return k
```

**Interviewer:** This approach looks good. What are the time and space complexity here?

**Interviewee:**

- **Time Complexity:** The time complexity remains `O(n)` because we are still traversing the entire array once.
- **Space Complexity:** The space complexity is reduced to `O(1)` since we are modifying the array in place and not using any extra space apart from a few variables.

**Interviewer:** Great job! Let's illustrate this with an example for better clarity.

### Example & Visual Representation

For `nums = [0,0,1,1,1,2,2,3,3,4]`:

- Initialize `k = 1` (since the first element is unique).
- Iterate over the array starting from index 1:
  - `i = 1`: `nums[1] == nums[0]`, so skip.
  - `i = 2`: `nums[2] != nums[1]`, update `nums[1]` to 1, increment `k` to 2.
  - `i = 3`: `nums[3] == nums[2]`, so skip.
  - `i = 4`: `nums[4] == nums[3]`, so skip.
  - `i = 5`: `nums[5] != nums[4]`, update `nums[2]` to 2, increment `k` to 3.
  - Continue this process...

This will result in the array being modified to `[0, 1, 2, 3, 4, _, _, _, _, _]` with `k = 5`.

Here's a visual representation:

```
Original array: [0, 0, 1, 1, 1, 2, 2, 3, 3, 4]

Step-by-step changes with two pointers i (iterations) and k (unique elements index):
i=1, k=1 -> [0, 0, 1, 1, 1, 2, 2, 3, 3, 4]  (no change)
i=2, k=1 -> [0, 1, 1, 1, 1, 2, 2, 3, 3, 4]  (k=2)
i=3, k=2 -> [0, 1, 1, 1, 1, 2, 2, 3, 3, 4]  (no change)
i=4, k=2 -> [0, 1, 1, 1, 1, 2, 2, 3, 3, 4]  (no change)
i=5, k=2 -> [0, 1, 2, 1, 1, 2, 2, 3, 3, 4]  (k=3)
i=6, k=3 -> [0, 1, 2, 1, 1, 2, 2, 3, 3, 4]  (no change)
i=7, k=3 -> [0, 1, 2, 3, 1, 2, 2, 3, 3, 4]  (k=4)
i=8, k=4 -> [0, 1, 2, 3, 1, 2, 2, 3, 3, 4]  (no change)
i=9, k=4 -> [0, 1, 2, 3, 4, 2, 2, 3, 3, 4]  (k=5)

Final array:     [0, 1, 2, 3, 4, _, _, _, _, _]  (k=5)
```

**Interviewer:** That’s a clear explanation and an efficient solution. Well done!
Certainly! I will provide the `removeDuplicates` function implementation with time and space complexity comments in each of the specified languages.

### C++
```cpp
class Solution {
public:
    int removeDuplicates(vector<int>& nums) {
        if (nums.empty()) return 0;
        int k = 1;  // Unique elements index
        for (int i = 1; i < nums.size(); ++i) {
            if (nums[i] != nums[k - 1]) {
                nums[k] = nums[i];
                ++k;
            }
        }
        return k;
    }
};

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Java
```java
class Solution {
    public int removeDuplicates(int[] nums) {
        if (nums.length == 0) return 0;
        int k = 1;  // Unique elements index
        for (int i = 1; i < nums.length; i++) {
            if (nums[i] != nums[k - 1]) {
                nums[k] = nums[i];
                k++;
            }
        }
        return k;
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Python
```python
class Solution(object):
    def removeDuplicates(self, nums):
        """
        :type nums: List[int]
        :rtype: int
        """
        if not nums:
            return 0
        k = 1  # Unique elements index
        for i in range(1, len(nums)):
            if nums[i] != nums[k - 1]:
                nums[k] = nums[i]
                k += 1
        return k

# Time Complexity: O(n)
# Space Complexity: O(1)
```

### Python3
```python
class Solution:
    def removeDuplicates(self, nums: List[int]) -> int:
        if not nums:
            return 0
        k = 1  # Unique elements index
        for i in range(1, len(nums)):
            if nums[i] != nums[k - 1]:
                nums[k] = nums[i]
                k += 1
        return k

# Time Complexity: O(n)
# Space Complexity: O(1)
```

### C
```c
int removeDuplicates(int* nums, int numsSize) {
    if (numsSize == 0) return 0;
    int k = 1;  // Unique elements index
    for (int i = 1; i < numsSize; i++) {
        if (nums[i] != nums[k - 1]) {
            nums[k] = nums[i];
            k++;
        }
    }
    return k;
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### C#
```csharp
public class Solution {
    public int RemoveDuplicates(int[] nums) {
        if (nums.Length == 0) return 0;
        int k = 1;  // Unique elements index
        for (int i = 1; i < nums.Length; i++) {
            if (nums[i] != nums[k - 1]) {
                nums[k] = nums[i];
                k++;
            }
        }
        return k;
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
var removeDuplicates = function(nums) {
    if (nums.length === 0) return 0;
    let k = 1;  // Unique elements index
    for (let i = 1; i < nums.length; i++) {
        if (nums[i] != nums[k - 1]) {
            nums[k] = nums[i];
            k++;
        }
    }
    return k;
};

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### TypeScript
```typescript
function removeDuplicates(nums: number[]): number {
    if (nums.length === 0) return 0;
    let k = 1;  // Unique elements index
    for (let i = 1; i < nums.length; i++) {
        if (nums[i] != nums[k - 1]) {
            nums[k] = nums[i];
            k++;
        }
    }
    return k;
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
    function removeDuplicates(&$nums) {
        if (count($nums) == 0) return 0;
        $k = 1;  // Unique elements index
        for ($i = 1; $i < count($nums); $i++) {
            if ($nums[$i] != $nums[$k - 1]) {
                $nums[$k] = $nums[$i];
                $k++;
            }
        }
        return $k;
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Swift
```swift
class Solution {
    func removeDuplicates(_ nums: inout [Int]) -> Int {
        if nums.isEmpty { return 0 }
        var k = 1  // Unique elements index
        for i in 1..<nums.count {
            if nums[i] != nums[k - 1] {
                nums[k] = nums[i]
                k += 1
            }
        }
        return k
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Kotlin
```kotlin
class Solution {
    fun removeDuplicates(nums: IntArray): Int {
        if (nums.isEmpty()) return 0
        var k = 1  // Unique elements index
        for (i in 1 until nums.size) {
            if (nums[i] != nums[k - 1]) {
                nums[k] = nums[i]
                k++
            }
        }
        return k
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Dart
```dart
class Solution {
  int removeDuplicates(List<int> nums) {
    if (nums.isEmpty) return 0;
    int k = 1;  // Unique elements index
    for (int i = 1; i < nums.length; i++) {
      if (nums[i] != nums[k - 1]) {
        nums[k] = nums[i];
        k++;
      }
    }
    return k;
  }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Go
```go
func removeDuplicates(nums []int) int {
    if len(nums) == 0 {
        return 0
    }
    k := 1  // Unique elements index
    for i := 1; i < len(nums); i++ {
        if nums[i] != nums[k - 1] {
            nums[k] = nums[i]
            k++
        }
    }
    return k
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Ruby
```ruby
# @param {Integer[]} nums
# @return {Integer}
def remove_duplicates(nums)
    return 0 if nums.empty?
    k = 1  # Unique elements index
    (1...nums.length).each do |i|
        if nums[i] != nums[k - 1]
            nums[k] = nums[i]
            k += 1
        end
    end
    k
end

# Time Complexity: O(n)
# Space Complexity: O(1)
```

### Scala
```scala
object Solution {
    def removeDuplicates(nums: Array[Int]): Int = {
        if (nums.isEmpty) return 0
        var k = 1  // Unique elements index
        for (i <- 1 until nums.length) {
            if (nums(i) != nums(k - 1)) {
                nums(k) = nums(i)
                k += 1
            }
        }
        k
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Rust
```rust
impl Solution {
    pub fn remove_duplicates(nums: &mut Vec<i32>) -> i32 {
        if nums.is_empty() {
            return 0;
        }
        let mut k: usize = 1;  // Unique elements index
        for i in 1..nums.len() {
            if nums[i] != nums[k - 1] as i32 {
                nums[k] = nums[i];
                k += 1;
            }
        }
        k as i32
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

These implementations ensure we traverse the array only once, leading to `O(n)` time complexity, with `O(1)` space complexity since we modify the array in place and don't use any additional data structures.

### Closing Statement

In our discussion, we tackled the problem of removing duplicates from a sorted array while maintaining the original order of elements and doing so in place without using extra space. By iterating through the array and using a two-pointer technique, we crafted an efficient solution with a time complexity of `O(n)` and a space complexity of `O(1)`. This solution was implemented across various programming languages to demonstrate its versatility and comprehensiveness.

### Similar Questions

1. **Remove Element:**
   - **Problem:** Given an array `nums` and a value `val`, remove all instances of `val` in-place and return the new length.
   - **Link:** [LeetCode - Remove Element](https://leetcode.com/problems/remove-element/)

2. **Move Zeroes:**
   - **Problem:** Given an array `nums`, move all `0`s to the end while maintaining the relative order of non-zero elements.
   - **Link:** [LeetCode - Move Zeroes](https://leetcode.com/problems/move-zeroes/)

3. **Remove Duplicates from Sorted List:**
   - **Problem:** Given the head of a sorted linked list, delete all duplicates such that each element appears only once.
   - **Link:** [LeetCode - Remove Duplicates from Sorted List](https://leetcode.com/problems/remove-duplicates-from-sorted-list/)

4. **Longest Substring Without Repeating Characters:**
   - **Problem:** Given a string `s`, find the length of the longest substring without repeating characters.
   - **Link:** [LeetCode - Longest Substring Without Repeating Characters](https://leetcode.com/problems/longest-substring-without-repeating-characters/)

5. **Kth Largest Element in an Array:**
   - **Problem:** Given an integer array `nums` and an integer `k`, return the `k`th largest element in the array.
   - **Link:** [LeetCode - Kth Largest Element in an Array](https://leetcode.com/problems/kth-largest-element-in-an-array/)

These questions offer opportunities to practice similar techniques in array manipulation, in-place modifications, and efficient use of pointers and indices, which are all valuable skills for tackling a range of algorithmic challenges.