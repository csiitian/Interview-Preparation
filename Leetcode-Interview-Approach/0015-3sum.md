### Interviewer and Interviewee Discussion

**Interviewer:** Let's discuss how you would approach solving the problem of finding all unique triplets in an array that sum up to zero. The array can contain both positive and negative integers. 

**Interviewee:** Sure, that's the classic "3-sum" problem. The goal is to find triplets that add up to zero, ensuring that each element in a triplet is distinct and that there are no duplicate triplets in the final output.

**Interviewer:** Exactly. How would you initially think about solving this problem? Perhaps you can start with a brute force approach.

**Interviewee:** 
At first glance, a brute force approach seems straightforward. We can use three nested loops to consider all combinations of triplets and check if their sum equals zero.

### Brute Force Approach

We'll iterate through each triplet using three nested loops. This means:

1. Loop `i` from 0 to `n-1`
2. Loop `j` from `i+1` to `n-1`
3. Loop `k` from `j+1` to `n-1`
4. Check if `nums[i] + nums[j] + nums[k] == 0`
5. If yes, add `[nums[i], nums[j], nums[k]]` to the result set, ensuring that duplicate triplets are not added.

Here’s how the brute force implementation in Python might look:

```python
def three_sum(nums):
    n = len(nums)
    result = set()
    for i in range(n):
        for j in range(i + 1, n):
            for k in range(j + 1, n):
                if nums[i] + nums[j] + nums[k] == 0:
                    triplet = tuple(sorted((nums[i], nums[j], nums[k])))
                    result.add(triplet)
    return list(result)
```

### Time and Space Complexity of Brute Force

**Interviewer:** Could you analyze the time and space complexity of this approach?

**Interviewee:** 
- **Time Complexity:** The brute force approach uses three nested loops, each ranging from 0 to `n`, where `n` is the length of the array. Therefore, its time complexity is `O(n^3)`.
- **Space Complexity:** The main space complexity comes from storing the unique triplets in the result set. In the worst case, there could be `O(n^3)` entries in the set due to the possible combinations, though typically it will be much less.

### Optimizing the Approach

**Interviewer:** This brute force method is quite inefficient. Can you think of a more optimized approach?

**Interviewee:** Yes, the time complexity can be greatly improved by using sorting and the two-pointer technique. Here’s the optimized plan:

1. **Sort the Array:** Sorting helps simplify the problem, especially with managing duplicates and utilizing the two-pointer technique.
2. **Iterate through the array:** Use a loop to fix the first element of the triplet.
3. **Two-Pointer Technique:** Use two pointers to find pairs that sum to the negation of the fixed element. The left pointer starts just after the fixed element, and the right pointer starts at the end of the array.
4. **Skip Duplicates:** Before and after finding a valid triplet, skip over any duplicate values.

Here’s how this optimized implementation looks in Python:

```python
def three_sum(nums):
    nums.sort()
    result = []
    for i in range(len(nums) - 2):
        if i > 0 and nums[i] == nums[i - 1]:
            continue  # skip duplicates
        left, right = i + 1, len(nums) - 1
        while left < right:
            triplet_sum = nums[i] + nums[left] + nums[right]
            if triplet_sum == 0:
                result.append([nums[i], nums[left], nums[right]])
                while left < right and nums[left] == nums[left + 1]:
                    left += 1
                while left < right and nums[right] == nums[right - 1]:
                    right -= 1
                left += 1
                right -= 1
            elif triplet_sum < 0:
                left += 1
            else:
                right -= 1
    return result
```

To visualize this, consider an example array `[-2, -1, 0, 1, 2]`. The sorted array helps us systematically check and skip duplicates efficiently.

### Time and Space Complexity of Optimized Approach

**Interviewer:** And what is the time and space complexity of this optimized approach?

**Interviewee:**
- **Time Complexity:** Sorting the array takes `O(n log n)`. The main loop runs `O(n)` times, and for each iteration, the two-pointer technique runs in `O(n)` time. Overall, this results in `O(n^2)` time complexity, which is a significant improvement over the brute force approach.
- **Space Complexity:** The space used mainly includes the sorted array and the result list. This results in `O(n)` space complexity for storing the results in the worst case.

**Interviewer:** Great, this is a much more efficient approach and should work well within the constraints. Thank you for the detailed walk-through!

**Interviewee:** My pleasure!
### C++

```cpp
class Solution {
public:
    vector<vector<int>> threeSum(vector<int>& nums) {
        sort(nums.begin(), nums.end());
        vector<vector<int>> result;
        for (int i = 0; i < nums.size() - 2; ++i) {
            if (i > 0 && nums[i] == nums[i - 1]) continue; // skip duplicates
            int left = i + 1, right = nums.size() - 1;
            while (left < right) {
                int sum = nums[i] + nums[left] + nums[right];
                if (sum == 0) {
                    result.push_back({nums[i], nums[left], nums[right]});
                    while (left < right && nums[left] == nums[left + 1]) left++; // skip duplicates
                    while (left < right && nums[right] == nums[right - 1]) right--; // skip duplicates
                    left++;
                    right--;
                } else if (sum < 0) {
                    left++;
                } else {
                    right--;
                }
            }
        }
        return result;
    }
};
```
### Java

```java
import java.util.*;

class Solution {
    public List<List<Integer>> threeSum(int[] nums) {
        Arrays.sort(nums);
        List<List<Integer>> result = new ArrayList<>();
        for (int i = 0; i < nums.length - 2; ++i) {
            if (i > 0 && nums[i] == nums[i - 1]) continue; // skip duplicates
            int left = i + 1, right = nums.length - 1;
            while (left < right) {
                int sum = nums[i] + nums[left] + nums[right];
                if (sum == 0) {
                    result.add(Arrays.asList(nums[i], nums[left], nums[right]));
                    while (left < right && nums[left] == nums[left + 1]) left++; // skip duplicates
                    while (left < right && nums[right] == nums[right - 1]) right--; // skip duplicates
                    left++;
                    right--;
                } else if (sum < 0) {
                    left++;
                } else {
                    right--;
                }
            }
        }
        return result;
    }
}
```

### Python

```python
class Solution(object):
    def threeSum(self, nums):
        """
        :type nums: List[int]
        :rtype: List[List[int]]
        """
        nums.sort()
        result = []
        for i in range(len(nums) - 2):
            if i > 0 and nums[i] == nums[i - 1]:
                continue  # skip duplicates
            left, right = i + 1, len(nums) - 1
            while left < right:
                triplet_sum = nums[i] + nums[left] + nums[right]
                if triplet_sum == 0:
                    result.append([nums[i], nums[left], nums[right]])
                    while left < right and nums[left] == nums[left + 1]:
                        left += 1
                    while left < right and nums[right] == nums[right - 1]:
                        right -= 1
                    left += 1
                    right -= 1
                elif triplet_sum < 0:
                    left += 1
                else:
                    right -= 1
        return result
```

### Python3

```python
class Solution:
    def threeSum(self, nums: List[int]) -> List[List[int]]:
        nums.sort()
        result = []
        for i in range(len(nums) - 2):
            if i > 0 and nums[i] == nums[i - 1]:
                continue  # skip duplicates
            left, right = i + 1, len(nums) - 1
            while left < right:
                triplet_sum = nums[i] + nums[left] + nums[right]
                if triplet_sum == 0:
                    result.append([nums[i], nums[left], nums[right]])
                    while left < right and nums[left] == nums[left + 1]:
                        left += 1
                    while left < right and nums[right] == nums[right - 1]:
                        right -= 1
                    left += 1
                    right -= 1
                elif triplet_sum < 0:
                    left += 1
                else:
                    right -= 1
        return result
```

### C

```c
#include <stdlib.h>

/**
 * Return an array of arrays of size *returnSize.
 * The sizes of the arrays are returned as *returnColumnSizes array.
 * Note: Both returned array and *columnSizes array must be malloced, assume caller calls free().
 */
int** threeSum(int* nums, int numsSize, int* returnSize, int** returnColumnSizes) {
    // Sort the numbers
    qsort(nums, numsSize, sizeof(int), cmp);

    // Result dynamic array
    int** result = (int**)malloc(1000 * sizeof(int*));
    *returnColumnSizes = (int*)malloc(1000 * sizeof(int));
    *returnSize = 0;

    for (int i = 0; i < numsSize - 2; ++i) {
        if (i > 0 && nums[i] == nums[i - 1]) continue; // skip duplicates

        int left = i + 1, right = numsSize - 1;
        while (left < right) {
            int sum = nums[i] + nums[left] + nums[right];
            if (sum == 0) {
                result[*returnSize] = (int*)malloc(3 * sizeof(int));
                result[*returnSize][0] = nums[i];
                result[*returnSize][1] = nums[left];
                result[*returnSize][2] = nums[right];
                (*returnColumnSizes)[*returnSize] = 3;
                *returnSize += 1;
                while (left < right && nums[left] == nums[left + 1]) left++; // skip duplicates
                while (left < right && nums[right] == nums[right - 1]) right--; // skip duplicates
                left++;
                right--;
            } else if (sum < 0) {
                left++;
            } else {
                right--;
            }
        }
    }
    return result;
}

int cmp(const void *a, const void *b) {
    return (*(int*)a - *(int*)b);
}
```

### C#

```csharp
using System;
using System.Collections.Generic;

public class Solution {
    public IList<IList<int>> ThreeSum(int[] nums) {
        Array.Sort(nums);
        IList<IList<int>> result = new List<IList<int>>();
        for (int i = 0; i < nums.Length - 2; ++i) {
            if (i > 0 && nums[i] == nums[i - 1]) continue; // skip duplicates
            int left = i + 1, right = nums.Length - 1;
            while (left < right) {
                int sum = nums[i] + nums[left] + nums[right];
                if (sum == 0) {
                    result.Add(new List<int>{nums[i], nums[left], nums[right]});
                    while (left < right && nums[left] == nums[left + 1]) left++; // skip duplicates
                    while (left < right && nums[right] == nums[right - 1]) right--; // skip duplicates
                    left++;
                    right--;
                } else if (sum < 0) {
                    left++;
                } else {
                    right--;
                }
            }
        }
        return result;
    }
}
```

### JavaScript

```javascript
/**
 * @param {number[]} nums
 * @return {number[][]}
 */
var threeSum = function(nums) {
    nums.sort((a, b) => a - b);
    const result = [];
    for (let i = 0; i < nums.length - 2; ++i) {
        if (i > 0 && nums[i] === nums[i - 1]) continue; // skip duplicates
        let left = i + 1, right = nums.length - 1;
        while (left < right) {
            const sum = nums[i] + nums[left] + nums[right];
            if (sum === 0) {
                result.push([nums[i], nums[left], nums[right]]);
                while (left < right && nums[left] === nums[left + 1]) left++; // skip duplicates
                while (left < right && nums[right] === nums[right - 1]) right--; // skip duplicates
                left++;
                right--;
            } else if (sum < 0) {
                left++;
            } else {
                right--;
            }
        }
    }
    return result;
};
```

### TypeScript

```typescript
function threeSum(nums: number[]): number[][] {
    nums.sort((a, b) => a - b);
    const result: number[][] = [];
    for (let i = 0; i < nums.length - 2; ++i) {
        if (i > 0 && nums[i] === nums[i - 1]) continue; // skip duplicates
        let left = i + 1, right = nums.length - 1;
        while (left < right) {
            const sum = nums[i] + nums[left] + nums[right];
            if (sum === 0) {
                result.push([nums[i], nums[left], nums[right]]);
                while (left < right && nums[left] === nums[left + 1]) left++; // skip duplicates
                while (left < right && nums[right] === nums[right - 1]) right--; // skip duplicates
                left++;
                right--;
            } else if (sum < 0) {
                left++;
            } else {
                right--;
            }
        }
    }
    return result;
};
```

### PHP

```php
class Solution {

    /**
     * @param Integer[] $nums
     * @return Integer[][]
     */
    function threeSum($nums) {
        sort($nums);
        $result = [];
        $n = count($nums);
        for ($i = 0; $i < $n - 2; ++$i) {
            if ($i > 0 && $nums[$i] == $nums[$i - 1]) continue; // skip duplicates
            $left = $i + 1;
            $right = $n - 1;
            while ($left < $right) {
                $sum = $nums[$i] + $nums[$left] + $nums[$right];
                if ($sum == 0) {
                    $result[] = [$nums[$i], $nums[$left], $nums[$right]];
                    while ($left < $right && $nums[$left] == $nums[$left + 1]) $left++; // skip duplicates
                    while ($left < $right && $nums[$right] == $nums[$right - 1]) $right--; // skip duplicates
                    $left++;
                    $right--;
                } elseif ($sum < 0) {
                    $left++;
                } else {
                    $right--;
                }
            }
        }
        return $result;
    }
}
```

### Swift

```swift
class Solution {
    func threeSum(_ nums: [Int]) -> [[Int]] {
        let nums = nums.sorted()
        var result = [[Int]]()
        for i in 0..<nums.count - 2 {
            if i > 0 && nums[i] == nums[i - 1] { continue }  // skip duplicates
            var left = i + 1
            var right = nums.count - 1
            while left < right {
                let sum = nums[i] + nums[left] + nums[right]
                if sum == 0 {
                    result.append([nums[i], nums[left], nums[right]])
                    while left < right && nums[left] == nums[left + 1] { left += 1 }  // skip duplicates
                    while left < right && nums[right] == nums[right - 1] { right -= 1 }  // skip duplicates
                    left += 1
                    right -= 1
                } else if sum < 0 {
                    left += 1
                } else {
                    right -= 1
                }
            }
        }
        return result
    }
}
```

### Kotlin

```kotlin
class Solution {
    fun threeSum(nums: IntArray): List<List<Int>> {
        nums.sort()
        val result = mutableListOf<List<Int>>()
        for (i in 0 until nums.size - 2) {
            if (i > 0 && nums[i] == nums[i - 1]) continue // skip duplicates
            var left = i + 1
            var right = nums.size - 1
            while (left < right) {
                val sum = nums[i] + nums[left] + nums[right]
                if (sum == 0) {
                    result.add(listOf(nums[i], nums[left], nums[right]))
                    while (left < right && nums[left] == nums[left + 1]) left++ // skip duplicates
                    while (left < right && nums[right] == nums[right - 1]) right-- // skip duplicates
                    left++
                    right--
                } else if (sum < 0) {
                    left++
                } else {
                    right--
                }
            }
        }
        return result
    }
}
```

### Dart

```dart
class Solution {
  List<List<int>> threeSum(List<int> nums) {
    nums.sort();
    List<List<int>> result = [];
    for (int i = 0; i < nums.length - 2; ++i) {
      if (i > 0 && nums[i] == nums[i - 1]) continue; // skip duplicates
      int left = i + 1, right = nums.length - 1;
      while (left < right) {
        int sum = nums[i] + nums[left] + nums[right];
        if (sum == 0) {
          result.add([nums[i], nums[left], nums[right]]);
          while (left < right && nums[left] == nums[left + 1]) left++; // skip duplicates
          while (left < right && nums[right] == nums[right - 1]) right--; // skip duplicates
          left++;
          right--;
        } else if (sum < 0) {
          left++;
        } else {
          right--;
        }
      }
    }
    return result;
  }
}
```

### Go

```go
import "sort"

func threeSum(nums []int) [][]int {
    sort.Ints(nums)
    var result [][]int
    for i := 0; i < len(nums)-2; i++ {
        if i > 0 && nums[i] == nums[i-1] {
            continue  // skip duplicates
        }
        left, right := i+1, len(nums)-1
        for left < right {
            sum := nums[i] + nums[left] + nums[right]
            if sum == 0 {
                result = append(result, []int{nums[i], nums[left], nums[right]})
                for left < right && nums[left] == nums[left+1] {
                    left++  // skip duplicates
                }
                for left < right && nums[right] == nums[right-1] {
                    right--  // skip duplicates
                }
                left++
                right--
            } else if sum < 0 {
                left++
            } else {
                right--
            }
        }
    }
    return result
}
```

### Ruby

```ruby
# @param {Integer[]} nums
# @return {Integer[][]}
def three_sum(nums)
    nums.sort!
    result = []
    (0..nums.length-3).each do |i|
        next if i > 0 && nums[i] == nums[i-1]  # skip duplicates
        left, right = i+1, nums.length-1
        while left < right
            sum = nums[i] + nums[left] + nums[right]
            if sum == 0
                result << [nums[i], nums[left], nums[right]]
                left += 1 while left < right && nums[left] == nums[left+1]
                right -= 1 while left < right && nums[right] == nums[right-1]
                left += 1
                right -= 1
            elsif sum < 0
                left += 1
            else
                right -= 1
            end
        end
    end
    result
end
```

### Scala

```scala
object Solution {
    def threeSum(nums: Array[Int]): List[List[Int]] = {
        scala.util.Sorting.quickSort(nums)
        var result: List[List[Int]] = List()
        for (i <- 0 until nums.length - 2) {
            if (i > 0 && nums(i) == nums(i - 1)) { // skip duplicates
                // Do nothing
            } else {
                var left = i + 1
                var right = nums.length - 1
                while (left < right) {
                    val sum = nums(i) + nums(left) + nums(right)
                    if (sum == 0) {
                        result = result :+ List(nums(i), nums(left), nums(right))
                        while (left < right && nums(left) == nums(left + 1)) left += 1  // skip duplicates
                        while (left < right && nums(right) == nums(right - 1)) right -= 1  // skip duplicates
                        left += 1
                        right -= 1
                    } else if (sum < 0) {
                        left += 1
                    } else {
                        right -= 1
                    }
                }
            }
        }
        result
    }
}
```

### Rust

```rust
impl Solution {
    pub fn three_sum(nums: Vec<i32>) -> Vec<Vec<i32>> {
        let mut nums = nums;
        nums.sort();
        let mut result = vec![];
        for i in 0..nums.len() - 2 {
            if i > 0 && nums[i] == nums[i - 1] { continue; } // skip duplicates
            let mut left = i + 1;
            let mut right = nums.len() - 1;
            while left < right {
                let sum = nums[i] + nums[left] + nums[right];
                if sum == 0 {
                    result.push(vec![nums[i], nums[left], nums[right]]);
                    while left < right && nums[left] == nums[left+1] { left += 1; } // skip duplicates
                    while left < right && nums[right] == nums[right-1] { right -= 1; } // skip duplicates
                    left += 1;
                    right -= 1;
                } else if sum < 0 {
                    left += 1;
                } else {
                    right -= 1;
                }
            }
        }
        result
    }
}
```


### Closing Statement

Great work on tackling this complex problem! We've walked through the logic of the classic "3-sum" problem, starting from a brute force approach and progressing to a more efficient solution using sorting and the two-pointer technique. This problem is a great way to practice thinking about optimizing both time and space complexity and is a common interview question due to its various intricacies and edge cases. You did a fantastic job converting the solution into multiple languages, which showcases your adaptability and understanding of different syntaxes and paradigms. Keep practicing these types of problems to continue sharpening your problem-solving and algorithmic skills.

### Similar Questions

Here are a few related questions that you might find interesting and beneficial to solve:

1. **Two Sum**: Given an array of integers and a target value, find two numbers such that they add up to the target.
   - [LeetCode 1 - Two Sum](https://leetcode.com/problems/two-sum/)

2. **4Sum**: Same as the 3-sum problem, but you need to find quadruplets in an array that sum up to a given target.
   - [LeetCode 18 - 4Sum](https://leetcode.com/problems/4sum/)

3. **3Sum Closest**: Given an array of integers, find three integers such that the sum is closest to a given target.
   - [LeetCode 16 - 3Sum Closest](https://leetcode.com/problems/3sum-closest/)

4. **3Sum Smaller**: Given an array of n integers, find the number of triplets whose sum is less than a given value.
   - [LeetCode 259 - 3Sum Smaller](https://leetcode.com/problems/3sum-smaller/)

5. **Combination Sum**: Given an array of distinct integers and a target value, find all unique combinations in the array where the integers sum to the target.
   - [LeetCode 39 - Combination Sum](https://leetcode.com/problems/combination-sum/)

6. **Pair with Given Sum in a Sorted Array**: Given a sorted array, find pairs that sum to a given target.
   - A variation of the Two Sum problem, but with a sorted array.

These questions not only build on the skills used in the 3-sum problem but also introduce additional strategies and constraints for you to consider. Happy coding and good luck with your future interviews!