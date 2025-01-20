### Interviewer and Interviewee Discussion

**Interviewer:** The problem we're going to discuss today is about modifying an array in-place. Given a sorted integer array `nums`, you need to remove duplicates such that each unique element appears at most twice. The relative order should remain the same, and you must do this with O(1) extra memory. Could you explain how you would approach this problem?

**Interviewee:** Sure. Since the array is sorted in non-decreasing order, we can take advantage of this property. Firstly, let's discuss a brute force approach to get a feel for the problem.

### Initial Thinking with Brute Force Approach

**Interviewee:** In a brute force approach, we could use a hash map or a dictionary to count occurrences of each element. Then, we can iterate over the dictionary to reconstruct the array, ensuring each element appears at most twice. Here are detailed steps:

1. Traverse through the array and use a dictionary to count the occurrences of each element.
2. Create a new array and iterate through the dictionary to add each element up to twice.
3. Copy the result back into `nums`.

### Time and Space Complexity of the Brute Force Approach

**Interviewer:** And what would be the time and space complexity of this approach?

**Interviewee:** 
- **Time Complexity:** The initial traversal to build the dictionary takes O(n) time. Reconstructing the array from the dictionary also takes O(n) time. Therefore, the total time complexity is O(n).
- **Space Complexity:** The dictionary requires O(n) additional space to store the counts, so the space complexity is O(n).

**Interviewer:** This solution uses extra space, which we are not allowed to do. Can you suggest a way to solve this with O(1) extra space?

### Optimized Approach Using Two Pointers

**Interviewee:** Yes, we can use a two-pointer technique to solve this problem in-place with O(1) extra space. Here's how:

1. We'll use two pointers: `i` to iterate over the array, and `j` to keep track of the position for the next element in the final array.

2. We'll also keep a count of occurrences for the current element.

3. As we iterate over the array with `i`, we:
  - Update the count for the current element.
  - If the count is less than or equal to 2, we copy the element to the `j`-th position and increment `j`.
  - Reset the count when we encounter a new element.

Here’s the step-by-step code for this approach:

```python
def removeDuplicates(nums):
    if len(nums) <= 2:
        return len(nums)
    
    j = 1
    count = 1
    
    for i in range(1, len(nums)):
        if nums[i] == nums[i - 1]:
            count += 1
        else:
            count = 1
        
        if count <= 2:
            j += 1
    
    return j
```

### Time and Space Complexity of the Optimized Approach

**Interviewer:** Great! What would be the time and space complexity of this approach?

**Interviewee:**
- **Time Complexity:** The optimized solution uses a single iteration over the array, so the time complexity is O(n).
- **Space Complexity:** We are not using any additional data structures that grow with input size, so the space complexity is O(1).

**Interviewer:** Can you draw this out to help visualise?

**Interviewee:** Sure! Below is a visual representation of the two-pointer approach:

```
Initial State:
nums = [1, 1, 1, 2, 2, 3]
i = 0, j = 0, count = 1

Iteration 1:
i -> 1, 1 - 1 = same => count = 2
nums = [1, 1, 1, 2, 2, 3]
          i
          j

Iteration 2:
i -> 1, 1 - 1 = same => count = 3 (skip since > 2)
nums = [1, 1, 1, 2, 2, 3]
             i
          j

Iteration 3:
i -> 2, 2 - 1 = different => count = 1
Copy element, nums[2] = nums[2]
nums = [1, 1, 2, 2, 2, 3]
               i
             j

Iteration 4:
i -> 2, 2 - 2 = same => count = 2
Copy element, nums[3] = nums[3]
nums = [1, 1, 2, 2, 2, 3]
                  i
                j

Iteration 5:
i -> 3, 3 - 2 = different => count = 1
Copy element, nums[4] = nums[5]
nums = [1, 1, 2, 2, 3, 3]
                     i
                   j
End of Loop:
j = 5

Final Array: [1, 1, 2, 2, 3]
```

**Interviewer:** That makes sense. Thank you for your explanation.
### C++

```cpp
class Solution {
public:
    int removeDuplicates(vector<int>& nums) {
        if (nums.size() <= 2) return nums.size();
        
        int j = 1; // Points to the position to place the next element
        int count = 1; // Count occurrences of the current element
        
        for (int i = 1; i < nums.size(); ++i) {
            if (nums[i] == nums[i - 1]) {
                count++;
            } else {
                count = 1;
            }
            
            if (count <= 2) {
                nums[j] = nums[i];
                j++;
            }
        }
        
        return j;
    }
};
```
**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Java

```java
class Solution {
    public int removeDuplicates(int[] nums) {
        if (nums.length <= 2) return nums.length;
        
        int j = 1;
        int count = 1;
        
        for (int i = 1; i < nums.length; i++) {
            if (nums[i] == nums[i - 1]) {
                count++;
            } else {
                count = 1;
            }
            
            if (count <= 2) {
                nums[j] = nums[i];
                j++;
            }
        }
        
        return j;
    }
}
```
**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Python

```python
class Solution(object):
    def removeDuplicates(self, nums):
        """
        :type nums: List[int]
        :rtype: int
        """
        if len(nums) <= 2:
            return len(nums)
        
        j = 1
        count = 1
        
        for i in range(1, len(nums)):
            if nums[i] == nums[i - 1]:
                count += 1
            else:
                count = 1
            
            if count <= 2:
                nums[j] = nums[i]
                j += 1
        
        return j
```
**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Python3

```python
class Solution:
    def removeDuplicates(self, nums: List[int]) -> int:
        if len(nums) <= 2:
            return len(nums)
        
        j = 1
        count = 1
        
        for i in range(1, len(nums)):
            if nums[i] == nums[i - 1]:
                count += 1
            else:
                count = 1
            
            if count <= 2:
                nums[j] = nums[i]
                j += 1
        
        return j
```
**Time Complexity:** O(n)
**Space Complexity:** O(1)

### C

```c
int removeDuplicates(int* nums, int numsSize) {
    if (numsSize <= 2) return numsSize;
    
    int j = 1;
    int count = 1;
    
    for (int i = 1; i < numsSize; i++) {
        if (nums[i] == nums[i-1]) {
            count++;
        } else {
            count = 1;
        }
        
        if (count <= 2) {
            nums[j] = nums[i];
            j++;
        }
    }
    
    return j;
}
```
**Time Complexity:** O(n)
**Space Complexity:** O(1)

### C#

```csharp
public class Solution {
    public int RemoveDuplicates(int[] nums) {
        if (nums.Length <= 2) return nums.Length;
        
        int j = 1;
        int count = 1;
        
        for (int i = 1; i < nums.Length; i++) {
            if (nums[i] == nums[i - 1]) {
                count++;
            } else {
                count = 1;
            }
            
            if (count <= 2) {
                nums[j] = nums[i];
                j++;
            }
        }
        
        return j;
    }
}
```
**Time Complexity:** O(n)
**Space Complexity:** O(1)

### JavaScript

```javascript
/**
 * @param {number[]} nums
 * @return {number}
 */
var removeDuplicates = function(nums) {
    if (nums.length <= 2) return nums.length;
    
    let j = 1;
    let count = 1;
    
    for (let i = 1; i < nums.length; i++) {
        if (nums[i] === nums[i - 1]) {
            count++;
        } else {
            count = 1;
        }
        
        if (count <= 2) {
            nums[j] = nums[i];
            j++;
        }
    }
    
    return j;
};
```
**Time Complexity:** O(n)
**Space Complexity:** O(1)

### TypeScript

```typescript
function removeDuplicates(nums: number[]): number {
    if (nums.length <= 2) return nums.length;
    
    let j = 1;
    let count = 1;
    
    for (let i = 1; i < nums.length; i++) {
        if (nums[i] === nums[i - 1]) {
            count++;
        } else {
            count = 1;
        }
        
        if (count <= 2) {
            nums[j] = nums[i];
            j++;
        }
    }
    
    return j;
}
```
**Time Complexity:** O(n)
**Space Complexity:** O(1)

### PHP

```php
class Solution {

    /**
     * @param Integer[] $nums
     * @return Integer
     */
    function removeDuplicates(&$nums) {
        if (count($nums) <= 2) return count($nums);
        
        $j = 1;
        $count = 1;
        
        for ($i = 1; $i < count($nums); $i++) {
            if ($nums[$i] == $nums[$i - 1]) {
                $count++;
            } else {
                $count = 1;
            }
            
            if ($count <= 2) {
                $nums[$j] = $nums[$i];
                $j++;
            }
        }
        
        return $j;
    }
}
```
**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Swift

```swift
class Solution {
    func removeDuplicates(_ nums: inout [Int]) -> Int {
        if nums.count <= 2 {
            return nums.count
        }
        
        var j = 1
        var count = 1
        
        for i in 1..<nums.count {
            if nums[i] == nums[i - 1] {
                count += 1
            } else {
                count = 1
            }
            
            if count <= 2 {
                nums[j] = nums[i]
                j += 1
            }
        }
        
        return j
    }
}
```
**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Kotlin

```kotlin
class Solution {
    fun removeDuplicates(nums: IntArray): Int {
        if (nums.size <= 2) return nums.size
        
        var j = 1
        var count = 1
        
        for (i in 1 until nums.size) {
            if (nums[i] == nums[i - 1]) {
                count++
            } else {
                count = 1
            }
            
            if (count <= 2) {
                nums[j] = nums[i]
                j++
            }
        }
        
        return j
    }
}
```
**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Dart

```dart
class Solution {
  int removeDuplicates(List<int> nums) {
    if (nums.length <= 2) return nums.length;
    
    int j = 1;
    int count = 1;
    
    for (int i = 1; i < nums.length; i++) {
      if (nums[i] == nums[i - 1]) {
        count++;
      } else {
        count = 1;
      }
      
      if (count <= 2) {
        nums[j] = nums[i];
        j++;
      }
    }
    
    return j;
  }
}
```
**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Go

```go
func removeDuplicates(nums []int) int {
    if len(nums) <= 2 {
        return len(nums)
    }
    
    j := 1
    count := 1
    
    for i := 1; i < len(nums); i++ {
        if nums[i] == nums[i-1] {
            count++
        } else {
            count = 1
        }
        
        if count <= 2 {
            nums[j] = nums[i]
            j++
        }
    }
    
    return j
}
```
**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Ruby

```ruby
# @param {Integer[]} nums
# @return {Integer}
def remove_duplicates(nums)
    return nums.length if nums.length <= 2
    
    j = 1
    count = 1
    
    (1...nums.length).each do |i|
        if nums[i] == nums[i-1]
            count += 1
        else
            count = 1
        end
        
        if count <= 2
            nums[j] = nums[i]
            j += 1
        end
    end
    
    return j
end
```
**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Scala

```scala
object Solution {
    def removeDuplicates(nums: Array[Int]): Int = {
        if (nums.length <= 2) return nums.length
        
        var j = 1
        var count = 1
        
        for (i <- 1 until nums.length) {
            if (nums(i) == nums(i - 1)) {
                count += 1
            } else {
                count = 1
            }
            
            if (count <= 2) {
                nums(j) = nums(i)
                j += 1
            }
        }
        
        j
    }
}
```
**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Rust

```rust
impl Solution {
    pub fn remove_duplicates(nums: &mut Vec<i32>) -> i32 {
        let len = nums.len();
        if len <= 2 {
            return len as i32;
        }
        
        let mut j = 1;
        let mut count = 1;
        
        for i in 1..len {
            if nums[i] == nums[i - 1] {
                count += 1;
            } else {
                count = 1;
            }
            
            if count <= 2 {
                nums[j] = nums[i];
                j += 1;
            }
        }
        
        j as i32
    }
}
```
**Time Complexity:** O(n)
**Space Complexity:** O(1)


### Closing Statement

**Interviewer:** Excellent job! You've clearly understood the problem and came up with an efficient in-place solution using the two-pointer technique. Your detailed explanation of both the brute force and optimized approaches, along with the considerations for time and space complexity, was very comprehensive. Implementing the solution in multiple programming languages reinforces the versatility of the algorithm. This is crucial for handling real-world problems where performance and memory utilization are key. Well done!

**Interviewee:** Thank you! It was a great exercise to think through the problem and optimize the solution. I appreciate the opportunity to discuss and implement the solution across various programming languages. 

### Similar Questions

Here are some similar questions that you might find useful for practice to reinforce these concepts:

1. **Remove Duplicates from Sorted Array II**:
   - Given a sorted array, remove the duplicates in place such that each element appears at most once and return the new length.

2. **Remove Duplicates from Sorted Array**:
   - Given a sorted array, remove the duplicates in place such that each element appears only once and return the new length.

3. **Move Zeroes**:
   - Given an array nums, write a function to move all 0's to the end of it while maintaining the relative order of the non-zero elements.

4. **Remove Element**:
   - Given an array nums and a value val, remove all instances of that value in place and return the new length.

5. **Partition Array into Disjoint Intervals**:
   - Given an array, partition it into disjoint intervals such that each element is part of at most one interval.

6. **Max Consecutive Ones II**:
   - Given a binary array, find the maximum number of consecutive 1s if you can flip at most one 0.

7. **Find All Duplicates in an Array**:
   - Given an array of integers, 1 ≤ `a[i]` ≤ `n` (n = size of array), some elements appear twice and others appear once. Find all the elements that appear twice in this array.

Practicing these questions will further improve your problem-solving skills and deepen your understanding of array manipulation and in-place algorithms. Good luck!