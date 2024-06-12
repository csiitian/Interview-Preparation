### Interviewer and Interviewee Discussion

**Interviewer:** Let's discuss a problem where you are given two sorted arrays, `nums1` and `nums2`, of sizes `m` and `n`, respectively. Your goal is to find the median of the two sorted arrays. The expected runtime complexity for an optimal solution should be \( O(\log(m+n)) \). How would you approach this problem?

**Interviewee:**
First, let's clarify what the median is. The median of a sorted list of numbers is the middle element if the number of elements is odd, or the average of the two middle elements if the number of elements is even.

To solve this, one straightforward approach would be to merge the two sorted arrays into a single sorted array and then find the median. However, this approach wouldn't meet the required time complexity but could serve as a good starting point for understanding the problem better.

### Initial Brute Force Approach

**Interviewee:**
For the brute force solution:
1. We can merge the two arrays into a single sorted array.
2. Then, we can find the median of this merged array.

Here are the steps to take:
1. Create a new array `merged` that has a length of `m+n`.
2. Use two pointers to iterate through `nums1` and `nums2`, inserting the smallest element from the two arrays into the merged array.
3. Once both arrays have been fully merged, calculate the median.

**Interviewer:** Could you discuss the time and space complexity for this brute force approach?

**Interviewee:**
Sure. For the brute force approach, both time and space complexities are as follows:

- **Time Complexity:** Merging two sorted arrays will take \( O(m + n) \) time.
- **Space Complexity:** We are creating an additional array `merged` of size \( m + n \), so the space complexity will be \( O(m + n) \).

### Optimizing the Approach

**Interviewer:** Given the constraints and the required time complexity of \( O(\log(m + n)) \), how can you optimize this solution?

**Interviewee:**
To achieve \( O(\log(m + n)) \) time complexity, we need to avoid fully merging the arrays. Instead, we can use a binary search approach on the smaller array to partition the arrays such that we can directly find the median.

Here are the key steps in the optimized approach:
1. Ensure that `nums1` is the smaller array.
2. Perform binary search on the smaller array to partition both arrays into two halves such that all elements in the left halves are less than or equal to all elements in the right halves.
3. Calculate the median based on the maximum elements of the left halves and the minimum elements of the right halves of these partitions.

Let's use an example to illustrate this:

### Example Illustration

Consider the arrays `nums1 = [1, 3]` and `nums2 = [2]`:

1. We make sure `nums1` is smaller, which it already is.
2. Partition `nums1` and `nums2` such that the left partition contains the same number of elements as the right partition (or one more if the total number is odd).
   Example Partition:
   ```
   nums1: | 1 [3]
   nums2: | 2
   ```
   Here, the left part would be `[1, 2]` and the right part would be `[3]`.

3. Compute the median:
   - Since there are an odd number of elements, the median is the maximum element of the left part: `median = max(1, 2) = 2`.

For the arrays `nums1 = [1, 2]` and `nums2 = [3, 4]`:

1. Ensure `nums1` is the smaller array:
2. Partition `nums1` and `nums2`:
   ```
   nums1: [1 | 2]
   nums2: [3 | 4]
   ```
   Here, the left part would be `[1, 2]` and the right part would be `[3, 4]`.

3. Compute the median:
   - Since there are an even number of elements, the median is the average of the maximum element of the left part and the minimum element of the right part: `median = (max(2, 1) + min(3, 4)) / 2 = 2.5`.

Drawing:

```
nums1: [ 1 | 3 ]
nums2: [ | 2 ] 
left_part: [1, 2]
right_part: [3]
```

In the corresponding solution, we adjust the partitions until we achieve the correct balance for both arrays.

### Complexity Analysis

- **Time Complexity:** The binary search algorithm runs in \( O(\log \min(m, n)) \).
- **Space Complexity:** The space complexity is \( O(1) \) as no additional data structures are required.

**Interviewer:** That sounds promising. Great job walking through both the initial brute force solution and the optimized approach.

**Interviewee:** Thank you!
Here is the solution for each provided language:

### C++

```cpp
class Solution {
public:
    double findMedianSortedArrays(vector<int>& nums1, vector<int>& nums2) {
        if (nums1.size() > nums2.size()) 
            return findMedianSortedArrays(nums2, nums1);
        
        int m = nums1.size();
        int n = nums2.size();
        int imin = 0, imax = m, half_len = (m + n + 1) / 2;
        double max_of_left, min_of_right;
        
        while (imin <= imax) {
            int i = (imin + imax) / 2;
            int j = half_len - i;
            if (i < m && nums1[i] < nums2[j - 1]) {
                imin = i + 1;
            } else if (i > 0 && nums1[i - 1] > nums2[j]) {
                imax = i - 1;
            } else {
                if (i == 0) max_of_left = nums2[j - 1];
                else if (j == 0) max_of_left = nums1[i - 1];
                else max_of_left = max(nums1[i - 1], nums2[j - 1]);
                
                if ((m + n) % 2 == 1) return max_of_left;
                
                if (i == m) min_of_right = nums2[j];
                else if (j == n) min_of_right = nums1[i];
                else min_of_right = min(nums1[i], nums2[j]);
                
                return (max_of_left + min_of_right) / 2.0;
            }
        }
        
        return 0.0;
    }
};
```

### Java

```java
class Solution {
    public double findMedianSortedArrays(int[] nums1, int[] nums2) {
        if (nums1.length > nums2.length) {
            return findMedianSortedArrays(nums2, nums1);
        }
        
        int m = nums1.length;
        int n = nums2.length;
        int imin = 0, imax = m, halfLen = (m + n + 1) / 2;
        double maxOfLeft, minOfRight;
        
        while (imin <= imax) {
            int i = (imin + imax) / 2;
            int j = halfLen - i;
            if (i < m && nums1[i] < nums2[j - 1]) {
                imin = i + 1;
            } else if (i > 0 && nums1[i - 1] > nums2[j]) {
                imax = i - 1;
            } else {
                if (i == 0) maxOfLeft = nums2[j - 1];
                else if (j == 0) maxOfLeft = nums1[i - 1];
                else maxOfLeft = Math.max(nums1[i - 1], nums2[j - 1]);
                
                if ((m + n) % 2 == 1) return maxOfLeft;
                
                if (i == m) minOfRight = nums2[j];
                else if (j == n) minOfRight = nums1[i];
                else minOfRight = Math.min(nums1[i], nums2[j]);
                
                return (maxOfLeft + minOfRight) / 2.0;
            }
        }
        
        return 0.0;
    }
}
```

### Python

```python
class Solution(object):
    def findMedianSortedArrays(self, nums1, nums2):
        if len(nums1) > len(nums2):
            nums1, nums2 = nums2, nums1
        
        m, n = len(nums1), len(nums2)
        imin, imax, half_len = 0, m, (m + n + 1) // 2
        
        while imin <= imax:
            i = (imin + imax) // 2
            j = half_len - i
            if i < m and nums1[i] < nums2[j - 1]:
                imin = i + 1
            elif i > 0 and nums1[i - 1] > nums2[j]:
                imax = i - 1
            else:
                if i == 0: max_of_left = nums2[j - 1]
                elif j == 0: max_of_left = nums1[i - 1]
                else: max_of_left = max(nums1[i - 1], nums2[j - 1])
                
                if (m + n) % 2 == 1:
                    return max_of_left
                
                if i == m: min_of_right = nums2[j]
                elif j == n: min_of_right = nums1[i]
                else: min_of_right = min(nums1[i], nums2[j])
                
                return (max_of_left + min_of_right) / 2.0
```

### Python3

```python
class Solution:
    def findMedianSortedArrays(self, nums1: List[int], nums2: List[int]) -> float:
        if len(nums1) > len(nums2):
            nums1, nums2 = nums2, nums1
        
        m, n = len(nums1), len(nums2)
        imin, imax, half_len = 0, m, (m + n + 1) // 2
        
        while imin <= imax:
            i = (imin + imax) // 2
            j = half_len - i
            if i < m and nums1[i] < nums2[j - 1]:
                imin = i + 1
            elif i > 0 and nums1[i - 1] > nums2[j]:
                imax = i - 1
            else:
                if i == 0: max_of_left = nums2[j - 1]
                elif j == 0: max_of_left = nums1[i - 1]
                else: max_of_left = max(nums1[i - 1], nums2[j - 1])
                
                if (m + n) % 2 == 1:
                    return max_of_left
                
                if i == m: min_of_right = nums2[j]
                elif j == n: min_of_right = nums1[i]
                else: min_of_right = min(nums1[i], nums2[j])
                
                return (max_of_left + min_of_right) / 2.0
```

### C

```c
double findMedianSortedArrays(int* nums1, int nums1Size, int* nums2, int nums2Size) {
    if (nums1Size > nums2Size) {
        return findMedianSortedArrays(nums2, nums2Size, nums1, nums1Size);
    }
    
    int m = nums1Size;
    int n = nums2Size;
    int imin = 0, imax = m, halfLen = (m + n + 1) / 2;
    double maxOfLeft, minOfRight;
    
    while (imin <= imax) {
        int i = (imin + imax) / 2;
        int j = halfLen - i;
        if (i < m && nums1[i] < nums2[j - 1]) {
            imin = i + 1;
        } else if (i > 0 && nums1[i - 1] > nums2[j]) {
            imax = i - 1;
        } else {
            if (i == 0) maxOfLeft = nums2[j - 1];
            else if (j == 0) maxOfLeft = nums1[i - 1];
            else maxOfLeft = fmax(nums1[i - 1], nums2[j - 1]);
            
            if ((m + n) % 2 == 1) return maxOfLeft;
            
            if (i == m) minOfRight = nums2[j];
            else if (j == n) minOfRight = nums1[i];
            else minOfRight = fmin(nums1[i], nums2[j]);
            
            return (maxOfLeft + minOfRight) / 2.0;
        }
    }
    
    return 0.0;
}
```

### C#

```csharp
public class Solution {
    public double FindMedianSortedArrays(int[] nums1, int[] nums2) {
        if (nums1.Length > nums2.Length) {
            return FindMedianSortedArrays(nums2, nums1);
        }
        
        int m = nums1.Length;
        int n = nums2.Length;
        int imin = 0, imax = m, halfLen = (m + n + 1) / 2;
        double maxOfLeft, minOfRight;
        
        while (imin <= imax) {
            int i = (imin + imax) / 2;
            int j = halfLen - i;
            if (i < m && nums1[i] < nums2[j - 1]) {
                imin = i + 1;
            } else if (i > 0 && nums1[i - 1] > nums2[j]) {
                imax = i - 1;
            } else {
                if (i == 0) maxOfLeft = nums2[j - 1];
                else if (j == 0) maxOfLeft = nums1[i - 1];
                else maxOfLeft = Math.Max(nums1[i - 1], nums2[j - 1]);
                
                if ((m + n) % 2 == 1) return maxOfLeft;
                
                if (i == m) minOfRight = nums2[j];
                else if (j == n) minOfRight = nums1[i];
                else minOfRight = Math.Min(nums1[i], nums2[j]);
                
                return (maxOfLeft + minOfRight) / 2.0;
            }
        }
        
        return 0.0;
    }
}
```

### JavaScript

```javascript
/**
 * @param {number[]} nums1
 * @param {number[]} nums2
 * @return {number}
 */
var findMedianSortedArrays = function(nums1, nums2) {
    if (nums1.length > nums2.length) {
        return findMedianSortedArrays(nums2, nums1);
    }
    
    let m = nums1.length;
    let n = nums2.length;
    let imin = 0, imax = m, halfLen = Math.floor((m + n + 1) / 2);
    let maxOfLeft, minOfRight;
    
    while (imin <= imax) {
        let i = Math.floor((imin + imax) / 2);
        let j = halfLen - i;
        if (i < m && nums1[i] < nums2[j - 1]) {
            imin = i + 1;
        } else if (i > 0 && nums1[i - 1] > nums2[j]) {
            imax = i - 1;
        } else {
            if (i === 0) maxOfLeft = nums2[j - 1];
            else if (j === 0) maxOfLeft = nums1[i - 1];
            else maxOfLeft = Math.max(nums1[i - 1], nums2[j - 1]);
            
            if ((m + n) % 2 === 1) return maxOfLeft;
            
            if (i === m) minOfRight = nums2[j];
            else if (j === n) minOfRight = nums1[i];
            else minOfRight = Math.min(nums1[i], nums2[j]);
            
            return (maxOfLeft + minOfRight) / 2.0;
        }
    }
    
    return 0.0;
};
```

### TypeScript

```typescript
function findMedianSortedArrays(nums1: number[], nums2: number[]): number {
    if (nums1.length > nums2.length) {
        return findMedianSortedArrays(nums2, nums1);
    }
    
    let m = nums1.length;
    let n = nums2.length;
    let imin = 0, imax = m, halfLen = Math.floor((m + n + 1) / 2);
    let maxOfLeft: number, minOfRight: number;
    
    while (imin <= imax) {
        let i = Math.floor((imin + imax) / 2);
        let j = halfLen - i;
        if (i < m && nums1[i] < nums2[j - 1]) {
            imin = i + 1;
        } else if (i > 0 && nums1[i - 1] > nums2[j]) {
            imax = i - 1;
        } else {
            if (i === 0) maxOfLeft = nums2[j - 1];
            else if (j === 0) maxOfLeft = nums1[i - 1];
            else maxOfLeft = Math.max(nums1[i - 1], nums2[j - 1]);
            
            if ((m + n) % 2 === 1) return maxOfLeft;
            
            if (i === m) minOfRight = nums2[j];
            else if (j === n) minOfRight = nums1[i];
            else minOfRight = Math.min(nums1[i], nums2[j]);
            
            return (maxOfLeft + minOfRight) / 2.0;
        }
    }
    
    return 0.0;
}
```

### PHP

```php
class Solution {

    /**
     * @param Integer[] $nums1
     * @param Integer[] $nums2
     * @return Float
     */
    function findMedianSortedArrays($nums1, $nums2) {
        if (count($nums1) > count($nums2)) {
            return $this->findMedianSortedArrays($nums2, $nums1);
        }
        
        $m = count($nums1);
        $n = count($nums2);
        $imin = 0;
        $imax = $m;
        $halfLen = intval(($m + $n + 1) / 2);
        $maxOfLeft = 0;
        $minOfRight = 0;
        
        while ($imin <= $imax) {
            $i = intval(($imin + $imax) / 2);
            $j = $halfLen - $i;
            if ($i < $m && $nums1[$i] < $nums2[$j - 1]) {
                $imin = $i + 1;
            } elseif ($i > 0 && $nums1[$i - 1] > $nums2[$j]) {
                $imax = $i - 1;
            } else {
                if ($i == 0) $maxOfLeft = $nums2[$j - 1];
                elseif ($j == 0) $maxOfLeft = $nums1[$i - 1];
                else $maxOfLeft = max($nums1[$i - 1], $nums2[$j - 1]);
                
                if (($m + $n) % 2 == 1) return $maxOfLeft;
                
                if ($i == $m) $minOfRight = $nums2[$j];
                elseif ($j == $n) $minOfRight = $nums1[$i];
                else $minOfRight = min($nums1[$i], $nums2[$j]);
                
                return ($maxOfLeft + $minOfRight) / 2.0;
            }
        }
        
        return 0.0;
    }
}
```

### Swift

```swift
class Solution {
    func findMedianSortedArrays(_ nums1: [Int], _ nums2: [Int]) -> Double {
        var A = nums1, B = nums2
        if A.count > B.count {
            swap(&A, &B)
        }
        let m = A.count
        let n = B.count
        var imin = 0, imax = m, halfLen = (m + n + 1) / 2
        
        while imin <= imax {
            let i = (imin + imax) / 2
            let j = halfLen - i
            if i < m && A[i] < B[j - 1] {
                imin = i + 1
            } else if i > 0 && A[i - 1] > B[j] {
                imax = i - 1
            } else {
                var maxOfLeft: Double
                if i == 0 {
                    maxOfLeft = Double(B[j - 1])
                } else if j == 0 {
                    maxOfLeft = Double(A[i - 1])
                } else {
                    maxOfLeft = Double(max(A[i - 1], B[j - 1]))
                }
                if (m + n) % 2 == 1 {
                    return maxOfLeft
                }
                
                var minOfRight: Double
                if i == m {
                    minOfRight = Double(B[j])
                } else if j == n {
                    minOfRight = Double(A[i])
                } else {
                    minOfRight = Double(min(A[i], B[j]))
                }
                
                return (maxOfLeft + minOfRight) / 2.0
            }
        }
        return 0.0
    }
}
```

### Kotlin

```kotlin
class Solution {
    fun findMedianSortedArrays(nums1: IntArray, nums2: IntArray): Double {
        var A = nums1
        var B = nums2
        if (A.size > B.size) {
            A = nums2
            B = nums1
        }
        val m = A.size
        val n = B.size
        var imin = 0
        var imax = m
        val halfLen = (m + n + 1) / 2
        
        while (imin <= imax) {
            val i = (imin + imax) / 2
            val j = halfLen - i
            if (i < m && A[i] < B[j - 1]) {
                imin = i + 1
            } else if (i > 0 && A[i - 1] > B[j]) {
                imax = i - 1
            } else {
                var maxOfLeft: Double
                maxOfLeft = when {
                    i == 0 -> B[j - 1].toDouble()
                    j == 0 -> A[i - 1].toDouble()
                    else -> max(A[i - 1], B[j - 1]).toDouble()
                }
                if ((m + n) % 2 == 1) {
                    return maxOfLeft
                }
                
                var minOfRight: Double
                minOfRight = when {
                    i == m -> B[j].toDouble()
                    j == n -> A[i].toDouble()
                    else -> min(A[i], B[j]).toDouble()
                }
                
                return (maxOfLeft + minOfRight) / 2.0
            }
        }
        return 0.0
    }
}
```
           
### Closing Statement

**Interviewer:** Great job! You've effectively tackled a complex problem and provided solutions across multiple programming languages. Your approach clearly demonstrated an understanding of both the brute force and optimized methods. The way you broke down each step and how you arrived at the O(log(m+n)) solution using binary search on the smaller array was impressive. This problem often comes up in interviews due to its requirement to understand both arrays and binary search thoroughly. Well done!

**Interviewee:** Thank you! I appreciate the opportunity to work through this problem and explain my thought process. I learned a lot during this discussion, particularly about optimizing solutions to meet specific time complexity requirements.

### Similar Questions

If you found this problem interesting or wish to further practice related concepts, here are some similar questions that you might find useful:

1. **Merge Two Sorted Lists:** Given two sorted linked lists, merge them into a single sorted linked list.
   - [LeetCode Link](https://leetcode.com/problems/merge-two-sorted-lists/)

2. **Find K-th Smallest Element in Two Sorted Arrays:** Given two sorted arrays, find the k-th smallest element in the union of these arrays.
   - [LeetCode Link](https://leetcode.com/problems/median-of-two-sorted-arrays/) (Generalized k-th element problem)

3. **Kth Largest Element in an Array:** Find the k-th largest element in an unsorted array.
   - [LeetCode Link](https://leetcode.com/problems/kth-largest-element-in-an-array/)

4. **Find Minimum in Rotated Sorted Array:** Suppose an array sorted in ascending order is rotated at some pivot unknown to you beforehand. Find the minimum element.
   - [LeetCode Link](https://leetcode.com/problems/find-minimum-in-rotated-sorted-array/)

5. **Search in Rotated Sorted Array:** Given a rotated sorted array, search for a particular target value.
   - [LeetCode Link](https://leetcode.com/problems/search-in-rotated-sorted-array/)

6. **Intersection of Two Arrays:** Find the intersection of two arrays.
   - [LeetCode Link](https://leetcode.com/problems/intersection-of-two-arrays/)

7. **Longest Common Subsequence:** Given two sequences, find the length of the longest subsequence present in both of them.
   - [LeetCode Link](https://leetcode.com/problems/longest-common-subsequence/)

Practicing these problems will help further enhance your skills in array manipulations, binary search algorithms, and merging techniques. Good luck!