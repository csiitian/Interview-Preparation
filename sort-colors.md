### Interviewer and Interviewee Discussion

**Interviewer:** Today, we'll be solving the problem of sorting an array of colors represented by integers. The task is similar to the famous Dutch National Flag problem proposed by Edsger Dijkstra. Given an array `nums` where each element is either 0 (red), 1 (white), or 2 (blue), we want to sort the array in-place so that objects of the same color are adjacent, and the colors are in the order red, white, and blue. You cannot use library sort functions. Before we dive into an optimized solution, let's discuss a brute-force approach. How would you approach this problem initially?

**Interviewee:** The brute force approach to solving this problem is fairly straightforward. We can simply use a sorting algorithm to sort the array. Since we only have three distinct values (0, 1, and 2), we can sort the array using the built-in sort function or implement a simple sorting algorithm ourselves.

**Interviewer:** That sounds reasonable. Could you talk about the time and space complexity of this approach?

**Interviewee:** Sure. If we use a built-in sort method like Timsort (Python's sort function), it will typically run in O(n log n) time complexity. The space complexity would generally be O(1) for in-place sorting. However, even with simpler sorting algorithms like Bubble Sort, the time complexity would still be O(n²), and again, the space complexity remains O(1).

Here's a quick sketch for clarity:
```
nums = [2, 0, 2, 1, 1, 0]

Sorted (Brute Force):
nums = [0, 0, 1, 1, 2, 2]
```

### Optimizing the Approach

**Interviewer:** Can we improve on this approach? Specifically, can we bring down the time complexity to O(n) and still maintain O(1) space complexity?

**Interviewee:** Yes, we can definitely optimize this using the Dutch National Flag algorithm. This approach will allow us to sort the array in a single pass (O(n) time complexity) and use constant space (O(1) space complexity). 

**Interviewer:** Great! Can you walk me through how the Dutch National Flag algorithm works?

**Interviewee:** Absolutely. The Dutch National Flag problem involves three pointers:
1. `low` – for the boundary of the section containing 0s.
2. `mid` – for the current element being examined.
3. `high` – for the boundary of the section containing 2s.

The goal is to traverse through the array with `mid` and rearrange elements as follows:
- If `nums[mid]` is 0, we swap it with `nums[low]` and increment both `low` and `mid`.
- If `nums[mid]` is 1, we just move `mid` forward.
- If `nums[mid]` is 2, we swap it with `nums[high]` and decrement `high`.

Here's an illustrative step-by-step:

Before any operation:
```
nums = [2, 0, 2, 1, 1, 0]
low = 0, mid = 0, high = 5
```

1. Swap `nums[mid]` with `nums[high]` because `nums[mid]` is 2:
```
nums = [0, 0, 2, 1, 1, 2]
low = 0, mid = 0, high = 4
```

2. Move `mid` forward because `nums[mid]` is 0:
```
nums = [0, 0, 2, 1, 1, 2]
low = 1, mid = 1, high = 4
```

3. Move `mid` forward again because `nums[mid]` is 0:
```
nums = [0, 0, 2, 1, 1, 2]
low = 2, mid = 2, high = 4
```

4. Swap `nums[mid]` with `nums[high]` because `nums[mid]` is 2:
```
nums = [0, 0, 1, 1, 2, 2]
low = 2, mid = 2, high = 3
```

5. Move `mid` forward because `nums[mid]` is 1:
```
nums = [0, 0, 1, 1, 2, 2]
low = 2, mid = 3, high = 3
```

6. Process is complete as `mid` exceeds `high`.

### Time and Space Complexity

Since each element is processed only once, the time complexity is O(n). The space complexity remains O(1) because we're only using a fixed number of extra variables.

### Final Code

```python
def sortColors(nums):
    low, mid, high = 0, 0, len(nums) - 1

    while mid <= high:
        if nums[mid] == 0:
            nums[low], nums[mid] = nums[mid], nums[low]
            low += 1
            mid += 1
        elif nums[mid] == 1:
            mid += 1
        else:
            nums[high], nums[mid] = nums[mid], nums[high]
            high -= 1
```

This implementation ensures the array is sorted in a single pass and uses constant extra space.
Sure! Let's implement the Dutch National Flag algorithm in each of the provided programming languages. The code will be wrapped in the given methods, and I'll also mention the time and space complexity explicitly.

### C++

```cpp
class Solution {
public:
    void sortColors(vector<int>& nums) {
        int low = 0, mid = 0, high = nums.size() - 1;
        while (mid <= high) {
            if (nums[mid] == 0) {
                swap(nums[low++], nums[mid++]);
            } else if (nums[mid] == 1) {
                mid++;
            } else {
                swap(nums[mid], nums[high--]);
            }
        }
    }
};

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Java

```java
class Solution {
    public void sortColors(int[] nums) {
        int low = 0, mid = 0, high = nums.length - 1;
        while (mid <= high) {
            if (nums[mid] == 0) {
                int temp = nums[low];
                nums[low] = nums[mid];
                nums[mid] = temp;
                low++;
                mid++;
            } else if (nums[mid] == 1) {
                mid++;
            } else {
                int temp = nums[mid];
                nums[mid] = nums[high];
                nums[high] = temp;
                high--;
            }
        }
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Python

```python
class Solution(object):
    def sortColors(self, nums):
        """
        :type nums: List[int]
        :rtype: None Do not return anything, modify nums in-place instead.
        """
        low, mid, high = 0, 0, len(nums) - 1
        while mid <= high:
            if nums[mid] == 0:
                nums[low], nums[mid] = nums[mid], nums[low]
                low += 1
                mid += 1
            elif nums[mid] == 1:
                mid += 1
            else:
                nums[high], nums[mid] = nums[mid], nums[high]
                high -= 1

# Time Complexity: O(n)
# Space Complexity: O(1)
```

### Python3

```python
class Solution:
    def sortColors(self, nums: List[int]) -> None:
        """
        Do not return anything, modify nums in-place instead.
        """
        low, mid, high = 0, 0, len(nums) - 1
        while mid <= high:
            if nums[mid] == 0:
                nums[low], nums[mid] = nums[mid], nums[low]
                low += 1
                mid += 1
            elif nums[mid] == 1:
                mid += 1
            else:
                nums[high], nums[mid] = nums[mid], nums[high]
                high -= 1

# Time Complexity: O(n)
# Space Complexity: O(1)
```

### C

```c
void sortColors(int* nums, int numsSize) {
    int low = 0, mid = 0, high = numsSize - 1;
    while (mid <= high) {
        if (nums[mid] == 0) {
            int temp = nums[low];
            nums[low] = nums[mid];
            nums[mid] = temp;
            low++;
            mid++;
        } else if (nums[mid] == 1) {
            mid++;
        } else {
            int temp = nums[mid];
            nums[mid] = nums[high];
            nums[high] = temp;
            high--;
        }
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### C#

```csharp
public class Solution {
    public void SortColors(int[] nums) {
        int low = 0, mid = 0, high = nums.Length - 1;
        while (mid <= high) {
            if (nums[mid] == 0) {
                int temp = nums[low];
                nums[low] = nums[mid];
                nums[mid] = temp;
                low++;
                mid++;
            } else if (nums[mid] == 1) {
                mid++;
            } else {
                int temp = nums[mid];
                nums[mid] = nums[high];
                nums[high] = temp;
                high--;
            }
        }
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### JavaScript

```javascript
/**
 * @param {number[]} nums
 * @return {void} Do not return anything, modify nums in-place instead.
 */
var sortColors = function(nums) {
    let low = 0, mid = 0, high = nums.length - 1;
    while (mid <= high) {
        if (nums[mid] === 0) {
            [nums[low], nums[mid]] = [nums[mid], nums[low]];
            low++;
            mid++;
        } else if (nums[mid] === 1) {
            mid++;
        } else {
            [nums[mid], nums[high]] = [nums[high], nums[mid]];
            high--;
        }
    }
};

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### TypeScript

```typescript
/**
 Do not return anything, modify nums in-place instead.
 */
function sortColors(nums: number[]): void {
    let low = 0, mid = 0, high = nums.length - 1;
    while (mid <= high) {
        if (nums[mid] === 0) {
            [nums[low], nums[mid]] = [nums[mid], nums[low]];
            low++;
            mid++;
        } else if (nums[mid] === 1) {
            mid++;
        } else {
            [nums[mid], nums[high]] = [nums[high], nums[mid]];
            high--;
        }
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### PHP

```php
class Solution {

    /**
     * @param Integer[] $nums
     * @return NULL
     */
    function sortColors(&$nums) {
        $low = 0;
        $mid = 0;
        $high = count($nums) - 1;
        while ($mid <= $high) {
            if ($nums[$mid] == 0) {
                list($nums[$low], $nums[$mid]) = [$nums[$mid], $nums[$low]];
                $low++;
                $mid++;
            } elseif ($nums[$mid] == 1) {
                $mid++;
            } else {
                list($nums[$mid], $nums[$high]) = [$nums[$high], $nums[$mid]];
                $high--;
            }
        }
    }

}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Swift

```swift
class Solution {
    func sortColors(_ nums: inout [Int]) {
        var low = 0, mid = 0, high = nums.count - 1
        while mid <= high {
            if nums[mid] == 0 {
                nums.swapAt(low, mid)
                low += 1
                mid += 1
            } else if nums[mid] == 1 {
                mid += 1
            } else {
                nums.swapAt(mid, high)
                high -= 1
            }
        }
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Kotlin

```kotlin
class Solution {
    fun sortColors(nums: IntArray): Unit {
        var low = 0
        var mid = 0
        var high = nums.size - 1
        while (mid <= high) {
            when (nums[mid]) {
                0 -> {
                    nums[low] = nums[mid].also { nums[mid] = nums[low] }
                    low++
                    mid++
                }
                1 -> mid++
                2 -> {
                    nums[high] = nums[mid].also { nums[mid] = nums[high] }
                    high--
                }
            }
        }
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Dart

```dart
class Solution {
  void sortColors(List<int> nums) {
    int low = 0, mid = 0, high = nums.length - 1;
    while (mid <= high) {
      if (nums[mid] == 0) {
        int temp = nums[low];
        nums[low] = nums[mid];
        nums[mid] = temp;
        low++;
        mid++;
      } else if (nums[mid] == 1) {
        mid++;
      } else {
        int temp = nums[mid];
        nums[mid] = nums[high];
        nums[high] = temp;
        high--;
      }
    }
  }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Go

```go
func sortColors(nums []int) {
    low, mid, high := 0, 0, len(nums) - 1
    for mid <= high {
        if nums[mid] == 0 {
            nums[low], nums[mid] = nums[mid], nums[low]
            low++
            mid++
        } else if nums[mid] == 1 {
            mid++
        } else {
            nums[mid], nums[high] = nums[high], nums[mid]
            high--
        }
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Ruby

```ruby
# @param {Integer[]} nums
# @return {Void} Do not return anything, modify nums in-place instead.
def sort_colors(nums)
    low, mid, high = 0, 0, nums.length - 1
    while mid <= high
        if nums[mid] == 0
            nums[low], nums[mid] = nums[mid], nums[low]
            low += 1
            mid += 1
        elsif nums[mid] == 1
            mid += 1
        else
            nums[high], nums[mid] = nums[mid], nums[high]
            high -= 1
        end
    end
end

# Time Complexity: O(n)
# Space Complexity: O(1)
```

### Scala

```scala
object Solution {
    def sortColors(nums: Array[Int]): Unit = {
        var low = 0
        var mid = 0
        var high = nums.length - 1
        while (mid <= high) {
            nums(mid) match {
                case 0 => {
                    val temp = nums(low)
                    nums(low) = nums(mid)
                    nums(mid) = temp
                    low += 1
                    mid += 1
                }
                case 1 => mid += 1
                case 2 => {
                    val temp = nums(mid)
                    nums(mid) = nums(high)
                    nums(high) = temp
                    high -= 1
                }
            }
        }
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Rust

```rust
impl Solution {
    pub fn sort_colors(nums: &mut Vec<i32>) {
        let (mut low, mut mid, mut high) = (0, 0, nums.len() - 1);
        while mid <= high {
            match nums[mid] {
                0 => {
                    nums.swap(low, mid);
                    low += 1;
                    mid += 1;
                }
                1 => mid += 1,
                2 => {
                    nums.swap(mid, high);
                    if high == 0 {
                        break;
                    }
                    high -= 1;
                }
                _ => (),
            }
        }
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

Each solution applies the Dutch National Flag algorithm for an in-place sort, achieving a time complexity of O(n) and a space complexity of O(1).


### Closing Statement

We started with the problem of sorting an array of colors represented by integers (0, 1, and 2) in a specific order using only in-place operations. First, we discussed a brute-force approach and its complexities. Although the brute-force approach is straightforward, its time complexity of O(n log n) can be improved.

We then introduced the Dutch National Flag algorithm, which optimizes the problem to achieve a linear time complexity of O(n) and constant space complexity of O(1). This algorithm uses three pointers (`low`, `mid`, and `high`) to efficiently partition the array into sections of 0s, 1s, and 2s in one pass.

We implemented this optimized algorithm in multiple programming languages, ensuring that the solution is both time-efficient and space-efficient.

### Similar Questions

1. **Sort Array by Parity**
   - *Description:* Given an array A of non-negative integers, return an array consisting of all the even elements of A, followed by all the odd elements. You may return any answer array that satisfies this condition.
   - *Link:* [LeetCode - Sort Array by Parity](https://leetcode.com/problems/sort-array-by-parity/)

2. **Sort an Array of 0s, 1s, and 2s**
   - *Description:* Given an array A[] consisting of 0s, 1s, and 2s, sort the array without using any sorting algorithm. This classic problem is similar to our current problem but with a more general application.
   - *Link:* [GeeksforGeeks - Sort an Array of 0s, 1s, and 2s](https://www.geeksforgeeks.org/sort-an-array-of-0s-1s-and-2s/)

3. **Wiggle Sort**
   - *Description:* Given an unsorted array `nums`, reorder it such that `nums[0] <= nums[1] >= nums[2] <= nums[3]...`. The input array should be reordered in-place.
   - *Link:* [LeetCode - Wiggle Sort](https://leetcode.com/problems/wiggle-sort/)

4. **Move Zeroes**
   - *Description:* Given an array `nums`, write a function to move all 0's to the end of it while maintaining the relative order of the non-zero elements.
   - *Link:* [LeetCode - Move Zeroes](https://leetcode.com/problems/move-zeroes/)

5. **Partition Labels**
   - *Description:* A string `S` of lowercase English letters is given. You need to partition this string into as many parts as possible so that each letter appears in at most one part, and return a list of integers representing the size of these parts.
   - *Link:* [LeetCode - Partition Labels](https://leetcode.com/problems/partition-labels/)

These related questions further explore sorting and partitioning arrays under various constraints, helping you get better at array manipulation and in-place sorting techniques.