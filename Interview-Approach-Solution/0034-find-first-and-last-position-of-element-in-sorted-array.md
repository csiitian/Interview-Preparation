### Interviewer and Interviewee Discussion

**Interviewer:**
Let's discuss your approach to this problem. How would you go about finding the starting and ending position of a given target value in a sorted array?

**Interviewee:**
Initially, I can think of a brute force approach where we iterate through the array to find the first and last occurrence of the target. This involves looping through the entire array and keeping track of the indices where the value matches the target.

**Interviewer:**
That sounds straightforward. Can you detail the steps involved in your brute force approach and its time complexity?

**Interviewee:**
Sure. Here's the brute force method step-by-step:

1. Initialize two variables, `start` and `end`, to store the starting and ending positions of the target, and set them to `-1` initially.
2. Loop through the array from the beginning to the end.
3. If we find an element equal to the target:
   - If it's the first occurrence, assign its index to `start`.
   - Keep updating `end` with the index of any matching element, since it will eventually hold the index of the last occurrence.
4. After the loop, if `start` remains `-1`, it means the target was not found, and we should return `[-1, -1]`.
5. Otherwise, return `[start, end]`.

The time complexity of this approach is `O(n)` because we may need to scan the entire array, and the space complexity is `O(1)` since we are only using a fixed amount of extra space.

**Interviewer:**
Good. However, the problem specifies that we need an `O(log n)` runtime complexity. Can you think of a more efficient solution that meets the required time complexity?

**Interviewee:**
Yes, we can improve the efficiency using a binary search approach, which inherently has a logarithmic complexity. Since the array is sorted, we can perform binary searches to find the first and last occurrence of the target value.

**Interviewer:**
Can you explain how you would implement the binary search to achieve this?

**Interviewee:**
Certainly. We'd employ two separate binary searches — one to find the leftmost index of the target and another to find the rightmost index.

1. **Finding the leftmost index (first occurrence):**
   - Perform a binary search. If we find the target, move towards the left half to check if there’s another instance of the target by adjusting the high pointer.
   - If we don’t find the target, adjust the pointers to continue the search.

2. **Finding the rightmost index (last occurrence):**
   - Again perform a binary search, but this time, if we find the target, we check the right half for any subsequent instances by adjusting the low pointer.

Let's illustrate how this works with an example.

### Example Visualization

Consider the array: `[5,7,7,8,8,10]` and target: `8`.

**Finding the leftmost index:**
- Start with `low = 0` and `high = 5`.
```
             mid
Array: [5,7,7,8,8,10]
Index:  0 1 2 3 4 5
```
- Check `mid` at index `2`; value is `7`. Move `low` to `3`.
- Next `mid` at index `4`; value is `8`. Move `high` to `3` to check left half.
- `mid` at index `3`; value is `8`. Move `high` to `2` to stop; found leftmost index `3`.

**Finding the rightmost index:**
- Start again with `low = 0` and `high = 5`.
- Check `mid` at index `2`; value is `7`. Move `low` to `3`.
- Next `mid` at index `4`; value is `8`. Move `low` to `5` to check right half.
- `mid` at index `5`; value is `10`. Move `high` to `4` to stop.
- Last valid `low = 4` value `8`, so rightmost index `4`.

Thus, the answer is `[3, 4]`.

### Code Implementation

```python
def searchRange(nums, target):
    def findLeft(nums, target):
        left, right = 0, len(nums) - 1
        while left <= right:
            mid = left + (right - left) // 2
            if nums[mid] < target:
                left = mid + 1
            else:
                right = mid - 1
        return left

    def findRight(nums, target):
        left, right = 0, len(nums) - 1
        while left <= right:
            mid = left + (right - left) // 2
            if nums[mid] <= target:
                left = mid + 1
            else:
                right = mid - 1
        return right

    left_index = findLeft(nums, target)
    right_index = findRight(nums, target)
    
    if left_index <= right_index and right_index < len(nums) and nums[left_index] == target and nums[right_index] == target:
        return [left_index, right_index]
    
    return [-1, -1]
```

**Interviewer:**
This is great. Your explanation is thorough and your code captures the logic needed to solve the problem within the `O(log n)` runtime complexity.
Sure! Let's implement the solution in each of the provided languages. I'll make sure to include time and space complexity comments in each snippet.

### C++
```cpp
class Solution {
public:
    vector<int> searchRange(vector<int>& nums, int target) {
        int findLeft(vector<int>& nums, int target) {
            int left = 0, right = nums.size() - 1;
            while (left <= right) {
                int mid = left + (right - left) / 2;
                if (nums[mid] < target) {
                    left = mid + 1;
                } else {
                    right = mid - 1;
                }
            }
            return left;
        }

        int findRight(vector<int>& nums, int target) {
            int left = 0, right = nums.size() - 1;
            while (left <= right) {
                int mid = left + (right - left) / 2;
                if (nums[mid] <= target) {
                    left = mid + 1;
                } else {
                    right = mid - 1;
                }
            }
            return right;
        }

        int left_index = findLeft(nums, target);
        int right_index = findRight(nums, target);
        
        if (left_index <= right_index && right_index < nums.size() && nums[left_index] == target && nums[right_index] == target) {
            return {left_index, right_index};
        }
        
        return {-1, -1};
    }
};
// Time Complexity: O(log n)
// Space Complexity: O(1)
```

### Java
```java
class Solution {
    public int[] searchRange(int[] nums, int target) {
        int findLeft(int[] nums, int target) {
            int left = 0, right = nums.length - 1;
            while (left <= right) {
                int mid = left + (right - left) / 2;
                if (nums[mid] < target) {
                    left = mid + 1;
                } else {
                    right = mid - 1;
                }
            }
            return left;
        }

        int findRight(int[] nums, int target) {
            int left = 0, right = nums.length - 1;
            while (left <= right) {
                int mid = left + (right - left) / 2;
                if (nums[mid] <= target) {
                    left = mid + 1;
                } else {
                    right = mid - 1;
                }
            }
            return right;
        }

        int left_index = findLeft(nums, target);
        int right_index = findRight(nums, target);
        
        if (left_index <= right_index && right_index < nums.length && nums[left_index] == target && nums[right_index] == target) {
            return new int[]{left_index, right_index};
        }

        return new int[]{-1, -1};
    }
}
// Time Complexity: O(log n)
// Space Complexity: O(1)
```

### Python
```python
class Solution(object):
    def searchRange(self, nums, target):
        """
        :type nums: List[int]
        :type target: int
        :rtype: List[int]
        """
        def findLeft(nums, target):
            left, right = 0, len(nums) - 1
            while left <= right:
                mid = left + (right - left) // 2
                if nums[mid] < target:
                    left = mid + 1
                else:
                    right = mid - 1
            return left

        def findRight(nums, target):
            left, right = 0, len(nums) - 1
            while left <= right:
                mid = left + (right - left) // 2
                if nums[mid] <= target:
                    left = mid + 1
                else:
                    right = mid - 1
            return right

        left_index = findLeft(nums, target)
        right_index = findRight(nums, target)
        
        if left_index <= right_index and right_index < len(nums) and nums[left_index] == target and nums[right_index] == target:
            return [left_index, right_index]
        return [-1, -1]

# Time Complexity: O(log n)
# Space Complexity: O(1)
```

### Python3
```python
class Solution:
    def searchRange(self, nums: List[int], target: int) -> List[int]:
        def findLeft(nums, target):
            left, right = 0, len(nums) - 1
            while left <= right:
                mid = left + (right - left) // 2
                if nums[mid] < target:
                    left = mid + 1
                else:
                    right = mid - 1
            return left

        def findRight(nums, target):
            left, right = 0, len(nums) - 1
            while left <= right:
                mid = left + (right - left) // 2
                if nums[mid] <= target:
                    left = mid + 1
                else:
                    right = mid - 1
            return right

        left_index = findLeft(nums, target)
        right_index = findRight(nums, target)
        
        if left_index <= right_index and right_index < len(nums) and nums[left_index] == target and nums[right_index] == target:
            return [left_index, right_index]
        
        return [-1, -1]

# Time Complexity: O(log n)
# Space Complexity: O(1)
```

### C
```c
#include <stdlib.h>

/**
 * Note: The returned array must be malloced, assume caller calls free().
 */
int* searchRange(int* nums, int numsSize, int target, int* returnSize){
    int findLeft(int* nums, int numsSize, int target) {
        int left = 0, right = numsSize - 1;
        while (left <= right) {
            int mid = left + (right - left) / 2;
            if (nums[mid] < target) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }
        return left;
    }
    
    int findRight(int* nums, int numsSize, int target) {
        int left = 0, right = numsSize - 1;
        while (left <= right) {
            int mid = left + (right - left) / 2;
            if (nums[mid] <= target) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }
        return right;
    }

    *returnSize = 2;
    int* result = (int*)malloc(2 * sizeof(int));
    int left_index = findLeft(nums, numsSize, target);
    int right_index = findRight(nums, numsSize, target);

    if (left_index <= right_index && right_index < numsSize && nums[left_index] == target && nums[right_index] == target) {
        result[0] = left_index;
        result[1] = right_index;
    } else {
        result[0] = -1;
        result[1] = -1;
    }
    
    return result;
}

// Time Complexity: O(log n)
// Space Complexity: O(1)
```

### C#
```csharp
public class Solution {
    public int[] SearchRange(int[] nums, int target) {
        int FindLeft(int[] nums, int target) {
            int left = 0, right = nums.Length - 1;
            while (left <= right) {
                int mid = left + (right - left) / 2;
                if (nums[mid] < target) {
                    left = mid + 1;
                } else {
                    right = mid - 1;
                }
            }
            return left;
        }

        int FindRight(int[] nums, int target) {
            int left = 0, right = nums.Length - 1;
            while (left <= right) {
                int mid = left + (right - left) / 2;
                if (nums[mid] <= target) {
                    left = mid + 1;
                } else {
                    right = mid - 1;
                }
            }
            return right;
        }

        int left_index = FindLeft(nums, target);
        int right_index = FindRight(nums, target);
        
        if (left_index <= right_index && right_index < nums.Length && nums[left_index] == target && nums[right_index] == target) {
            return new int[] { left_index, right_index };
        }
        
        return new int[] { -1, -1 };
    }
}
// Time Complexity: O(log n)
// Space Complexity: O(1)
```

### JavaScript
```javascript
/**
 * @param {number[]} nums
 * @param {number} target
 * @return {number[]}
 */
var searchRange = function(nums, target) {
    function findLeft(nums, target) {
        let left = 0, right = nums.length - 1;
        while (left <= right) {
            let mid = Math.floor(left + (right - left) / 2);
            if (nums[mid] < target) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }
        return left;
    }

    function findRight(nums, target) {
        let left = 0, right = nums.length - 1;
        while (left <= right) {
            let mid = Math.floor(left + (right - left) / 2);
            if (nums[mid] <= target) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }
        return right;
    }

    let left_index = findLeft(nums, target);
    let right_index = findRight(nums, target);

    if (left_index <= right_index && right_index < nums.length && nums[left_index] === target && nums[right_index] === target) {
        return [left_index, right_index];
    }
    return [-1, -1];
};
// Time Complexity: O(log n)
// Space Complexity: O(1)
```

### TypeScript
```typescript
function searchRange(nums: number[], target: number): number[] {
    function findLeft(nums: number[], target: number): number {
        let left = 0, right = nums.length - 1;
        while (left <= right) {
            let mid = Math.floor(left + (right - left) / 2);
            if (nums[mid] < target) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }
        return left;
    }

    function findRight(nums: number[], target: number): number {
        let left = 0, right = nums.length - 1;
        while (left <= right) {
            let mid = Math.floor(left + (right - left) / 2);
            if (nums[mid] <= target) {
                left = mid + 1;
            } else {
                right = mid - 1;
            }
        }
        return right;
    }

    let left_index = findLeft(nums, target);
    let right_index = findRight(nums, target);

    if (left_index <= right_index && right_index < nums.length && nums[left_index] === target && nums[right_index] === target) {
        return [left_index, right_index];
    }
    return [-1, -1];
}
// Time Complexity: O(log n)
// Space Complexity: O(1)
```

### PHP
```php
class Solution {
    /**
     * @param Integer[] $nums
     * @param Integer $target
     * @return Integer[]
     */
    function searchRange($nums, $target) {
        function findLeft($nums, $target) {
            $left = 0;
            $right = count($nums) - 1;
            while ($left <= $right) {
                $mid = (int)(($left + ($right - $left) / 2));
                if ($nums[$mid] < $target) {
                    $left = $mid + 1;
                } else {
                    $right = $mid - 1;
                }
            }
            return $left;
        }

        function findRight($nums, $target) {
            $left = 0;
            $right = count($nums) - 1;
            while ($left <= $right) {
                $mid = (int)(($left + ($right - $left) / 2));
                if ($nums[$mid] <= $target) {
                    $left = $mid + 1;
                } else {
                    $right = $mid - 1;
                }
            }
            return $right;
        }

        $left_index = findLeft($nums, $target);
        $right_index = findRight($nums, $target);

        if ($left_index <= $right_index && $right_index < count($nums) && $nums[$left_index] == $target && $nums[$right_index] == $target) {
            return [$left_index, $right_index];
        }

        return [-1, -1];
    }
}
// Time Complexity: O(log n)
// Space Complexity: O(1)
```

### Swift
```swift
class Solution {
    func searchRange(_ nums: [Int], _ target: Int) -> [Int] {
        func findLeft(_ nums: [Int], _ target: Int) -> Int {
            var left = 0, right = nums.count - 1
            while left <= right {
                let mid = left + (right - left) / 2
                if nums[mid] < target {
                    left = mid + 1
                } else {
                    right = mid - 1
                }
            }
            return left
        }

        func findRight(_ nums: [Int], _ target: Int) -> Int {
            var left = 0, right = nums.count - 1
            while left <= right {
                let mid = left + (right - left) / 2
                if nums[mid] <= target {
                    left = mid + 1
                } else {
                    right = mid - 1
                }
            }
            return right
        }

        let left_index = findLeft(nums, target)
        let right_index = findRight(nums, target)
        
        if left_index <= right_index && right_index < nums.count && nums[left_index] == target && nums[right_index] == target {
            return [left_index, right_index]
        }
        
        return [-1, -1]
    }
}
// Time Complexity: O(log n)
// Space Complexity: O(1)
```

### Kotlin
```kotlin
class Solution {
    fun searchRange(nums: IntArray, target: Int): IntArray {
        fun findLeft(nums: IntArray, target: Int): Int {
            var left = 0
            var right = nums.size - 1
            while (left <= right) {
                val mid = left + (right - left) / 2
                if (nums[mid] < target) {
                    left = mid + 1
                } else {
                    right = mid - 1
                }
            }
            return left
        }

        fun findRight(nums: IntArray, target: Int): Int {
            var left = 0
            var right = nums.size - 1
            while (left <= right) {
                val mid = left + (right - left) / 2
                if (nums[mid] <= target) {
                    left = mid + 1
                } else {
                    right = mid - 1
                }
            }
            return right
        }

        val left_index = findLeft(nums, target)
        val right_index = findRight(nums, target)

        if (left_index <= right_index && right_index < nums.size && nums[left_index] == target && nums[right_index] == target) {
            return intArrayOf(left_index, right_index)
        }

        return intArrayOf(-1, -1)
    }
}
// Time Complexity: O(log n)
// Space Complexity: O(1)
```

### Dart
```dart
class Solution {
  List<int> searchRange(List<int> nums, int target) {
    int findLeft(List<int> nums, int target) {
      int left = 0, right = nums.length - 1;
      while (left <= right) {
        int mid = left + (right - left) ~/ 2;
        if (nums[mid] < target) {
          left = mid + 1;
        } else {
          right = mid - 1;
        }
      }
      return left;
    }

    int findRight(List<int> nums, int target) {
      int left = 0, right = nums.length - 1;
      while (left <= right) {
        int mid = left + (right - left) ~/ 2;
        if (nums[mid] <= target) {
          left = mid + 1;
        } else {
          right = mid - 1;
        }
      }
      return right;
    }

    int left_index = findLeft(nums, target);
    int right_index = findRight(nums, target);

    if (left_index <= right_index && right_index < nums.length && nums[left_index] == target && nums[right_index] == target) {
      return [left_index, right_index];
    }
    
    return [-1, -1];
  }
}
// Time Complexity: O(log n)
// Space Complexity: O(1)
```

### Go
```go
func searchRange(nums []int, target int) []int {
    findLeft := func(nums []int, target int) int {
        left, right := 0, len(nums)-1
        for left <= right {
            mid := left + (right-left)/2
            if nums[mid] < target {
                left = mid + 1
            } else {
                right = mid - 1
            }
        }
        return left
    }
    
    findRight := func(nums []int, target int) int {
        left, right := 0, len(nums)-1
        for left <= right {
            mid := left + (right-left)/2
            if nums[mid] <= target {
                left = mid + 1
            } else {
                right = mid - 1
            }
        }
        return right
    }

    left_index := findLeft(nums, target)
    right_index := findRight(nums, target)

    if left_index <= right_index && right_index < len(nums) && nums[left_index] == target && nums[right_index] == target {
        return []int{left_index, right_index}
    }

    return []int{-1, -1}
}
// Time Complexity: O(log n)
// Space Complexity: O(1)
```

### Closing Statement

In this discussion, we've explored an efficient approach to finding the starting and ending positions of a target value in a sorted array. Initially, we discussed and analyzed the brute force method, which has a linear time complexity of `O(n)`. However, given the problem's requirement for an `O(log n)` runtime complexity, we shifted focus to an optimized solution using binary search.

The binary search approach leverages the sorted nature of the array to find the leftmost and rightmost indices of the target efficiently. We implemented this solution in multiple programming languages, ensuring the approach is consistent and accurate across different platforms.

Here is a summary of the key takeaways:
- **Binary Search Efficiency**: The optimized binary search approach yields `O(log n)` time complexity, making it suitable for large datasets.
- **Space Complexity**: Each implementation maintains an `O(1)` space complexity by using a minimal amount of extra space.
- **Edge Cases**: The solution effectively handles edge cases, including empty arrays and targets not present in the array.

By understanding and applying binary search techniques, we meet the problem's stringent performance requirements and provide a robust solution to efficiently locate the target range in a sorted array.

### Similar Questions

Here are some similar questions that you might find interesting:

1. **Find Peak Element**: Given an array of integers, find any peak element. A peak element is an element that is greater than its neighbors.
   - [Link to Problem](https://leetcode.com/problems/find-peak-element/)

2. **First Bad Version**: You are a product manager and currently leading a team to develop a new product. Unfortunately, the latest version of your product fails the quality check. Find the first bad version in a sorted array of versions.
   - [Link to Problem](https://leetcode.com/problems/first-bad-version/)

3. **Kth Smallest Element in a Sorted Matrix**: Given an `n x n` matrix where each row and column is sorted in ascending order, find the k-th smallest element in the matrix.
   - [Link to Problem](https://leetcode.com/problems/kth-smallest-element-in-a-sorted-matrix/)

4. **Search in Rotated Sorted Array**: Given the array `nums` after rotation and an integer `target`, return the index of `target` in `nums`, or `-1` if it is not present.
   - [Link to Problem](https://leetcode.com/problems/search-in-rotated-sorted-array/)

5. **Find Minimum in Rotated Sorted Array**: Suppose an array is sorted in ascending order, but then rotated at an unknown pivot. Find the minimum element in the array.
   - [Link to Problem](https://leetcode.com/problems/find-minimum-in-rotated-sorted-array/)

6. **Count of Smaller Numbers After Self**: Given an integer array `nums`, return an integer array `result` where `result[i]` is the number of smaller elements to the right of `nums[i]`.
   - [Link to Problem](https://leetcode.com/problems/count-of-smaller-numbers-after-self/)

These questions further explore variations and applications of binary search, providing a solid foundation for mastering search algorithms in different scenarios.