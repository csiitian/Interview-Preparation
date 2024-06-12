### Interviewer and Interviewee Discussion

**Interviewer**: Let's discuss the question. You are given an array of integers. You need to determine if it can be partitioned into three non-empty parts with equal sums. Do you understand the problem?

**Interviewee**: Yes, the task is to find if the array can be split into three parts such that each part has the same sum. We need to find indices `i` and `j` where each of the three subarrays `[0..i]`, `[i+1..j-1]`, and `[j..n-1]` has the same sum.

**Interviewer**: Correct. Let's start by discussing a brute force approach to solve this problem.

### Brute Force Approach

**Interviewee**: For a brute force approach:
1. We would iterate through all possible pairs of indices `(i, j)`.
2. For each pair, we would check if the sum of the elements from the start to `i`, `i+1` to `j-1`, and `j` to the end are all equal.
3. If we find such a pair, we return `true`. Otherwise, after all pairs are checked, we return `false`.

**Interviewer**: Let's discuss the time complexity of this approach.

**Interviewee**: This brute force approach would require two nested loops to go through all pairs `(i, j)`. Inside the inner loop, we would calculate the sums, which could take O(n) in the worst case.

- So the outer loop runs `n` times and the inner loop runs `n` times for each iteration of the outer loop.
- This gives us a time complexity of O(n^2).
- The additional sum computation within the loops would further give it an O(n^3) time complexity, making it very inefficient for larger arrays.

**Interviewer**: Great, what about space complexity?

**Interviewee**: The space complexity is O(1) as we are only using a constant amount of extra space for storing indices and sums, not considering input storage.

**Interviewer**: Can we optimize this approach?

**Interviewee**: Yes, definitely. Let's consider a more efficient approach:

### Optimized Approach

**Interviewee**: To optimize, we can utilize the fact that the sum of elements in each of the three parts must be `totalSum / 3`, given that `totalSum` is divisible by 3:
1. First, calculate the total sum of the array.
2. If the total sum is not divisible by 3, return `false` immediately.
3. If it is divisible, we aim for each partition to sum up to `totalSum / 3`.
4. Use a single pass to find these partitions. Track the cumulative sum and count how many times we reach `totalSum / 3`.

**Interviewer**: Could you show the logic in code?

**Interviewee**: Sure! Here's how it would look:

```python
def canThreePartsEqualSum(arr):
    totalSum = sum(arr)
    if totalSum % 3 != 0:
        return False
    
    target = totalSum // 3
    currentSum = 0
    count = 0
    
    for num in arr:
        currentSum += num
        if currentSum == target:
            count += 1
            currentSum = 0
        if count == 3:
            return True
    
    return False
```

**Interviewer**: Let's analyze the complexity of this approach.

**Interviewee**:
- **Time Complexity**: This approach only needs a single pass through the array to compute the sums and check partitions, making it O(n).
- **Space Complexity**: We are using a constant amount of extra space, so it's O(1).

### Diagram Illustration

**Interviewee**: Here's a simple illustration to explain the optimized approach:
```
Example: arr = [3, 3, 6, 5, -2, 2, 5, 1, -9, 4]
- Total Sum = 18
- Each partition should sum to 6 (18 / 3)

         __________________
        | 3 | 3 | 6 | 5 | -2 | 2 | 5 | 1 | -9 | 4 |
        |_____|______|______|______________________|
          P1     P2      P3
- Pass through the array and keep a running sum:
    - At index 1: running_sum = 6 (1st partition found)
    - At index 5: running_sum = 6 (2nd partition found)
    - Continue until the end and check if we can make three partitions

```

### Conclusion
- We iterated once through the array, keeping track of partition sums.
- This efficient solution runs in linear time and uses constant additional space.
Certainly! Here’s a consistent solution for the problem in each of the provided languages, annotating with the expected time and space complexity.

### C++
```cpp
class Solution {
public:
    bool canThreePartsEqualSum(vector<int>& arr) {
        int totalSum = accumulate(arr.begin(), arr.end(), 0);
        if (totalSum % 3 != 0) return false;
        
        int target = totalSum / 3, currentSum = 0, count = 0;
        for (int num : arr) {
            currentSum += num;
            if (currentSum == target) {
                count++;
                currentSum = 0;
            }
        }
        
        return count >= 3;
    }
};
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Java
```java
class Solution {
    public boolean canThreePartsEqualSum(int[] arr) {
        int totalSum = 0;
        for (int num : arr) totalSum += num;
        if (totalSum % 3 != 0) return false;
        
        int target = totalSum / 3, currentSum = 0, count = 0;
        for (int num : arr) {
            currentSum += num;
            if (currentSum == target) {
                count++;
                currentSum = 0;
            }
        }
        
        return count >= 3;
    }
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Python
```python
class Solution(object):
    def canThreePartsEqualSum(self, arr):
        """
        :type arr: List[int]
        :rtype: bool
        """
        totalSum = sum(arr)
        if totalSum % 3 != 0:
            return False
        
        target = totalSum // 3
        currentSum = 0
        count = 0
        
        for num in arr:
            currentSum += num
            if currentSum == target:
                count += 1
                currentSum = 0
        
        return count >= 3
# Time Complexity: O(n)
# Space Complexity: O(1)
```

### Python3
```python
class Solution:
    def canThreePartsEqualSum(self, arr: List[int]) -> bool:
        totalSum = sum(arr)
        if totalSum % 3 != 0:
            return False
        
        target = totalSum // 3
        currentSum = 0
        count = 0
        
        for num in arr:
            currentSum += num
            if currentSum == target:
                count += 1
                currentSum = 0
        
        return count >= 3
# Time Complexity: O(n)
# Space Complexity: O(1)
```

### C
```c
bool canThreePartsEqualSum(int* arr, int arrSize) {
    int totalSum = 0;
    for (int i = 0; i < arrSize; i++) totalSum += arr[i];
    if (totalSum % 3 != 0) return false;
    
    int target = totalSum / 3, currentSum = 0, count = 0;
    for (int i = 0; i < arrSize; i++) {
        currentSum += arr[i];
        if (currentSum == target) {
            count++;
            currentSum = 0;
        }
    }
    
    return count >= 3;
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### C#
```csharp
public class Solution {
    public bool CanThreePartsEqualSum(int[] arr) {
        int totalSum = 0;
        foreach (int num in arr) totalSum += num;
        if (totalSum % 3 != 0) return false;
        
        int target = totalSum / 3, currentSum = 0, count = 0;
        foreach (int num in arr) {
            currentSum += num;
            if (currentSum == target) {
                count++;
                currentSum = 0;
            }
        }
        
        return count >= 3;
    }
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### JavaScript
```javascript
/**
 * @param {number[]} arr
 * @return {boolean}
 */
var canThreePartsEqualSum = function(arr) {
    let totalSum = arr.reduce((acc, curr) => acc + curr, 0);
    if (totalSum % 3 !== 0) return false;
    
    let target = Math.floor(totalSum / 3), currentSum = 0, count = 0;
    for (let num of arr) {
        currentSum += num;
        if (currentSum === target) {
            count++;
            currentSum = 0;
        }
    }
    
    return count >= 3;
};
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### TypeScript
```typescript
function canThreePartsEqualSum(arr: number[]): boolean {
    const totalSum = arr.reduce((acc, curr) => acc + curr, 0);
    if (totalSum % 3 !== 0) return false;
    
    const target = Math.floor(totalSum / 3);
    let currentSum = 0, count = 0;
    for (let num of arr) {
        currentSum += num;
        if (currentSum === target) {
            count++;
            currentSum = 0;
        }
    }
    
    return count >= 3;
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### PHP
```php
class Solution {
    /**
     * @param Integer[] $arr
     * @return Boolean
     */
    function canThreePartsEqualSum($arr) {
        $totalSum = array_sum($arr);
        if ($totalSum % 3 != 0) return false;
        
        $target = intdiv($totalSum, 3);
        $currentSum = 0;
        $count = 0;
        
        foreach ($arr as $num) {
            $currentSum += $num;
            if ($currentSum == $target) {
                $count++;
                $currentSum = 0;
            }
        }
        
        return $count >= 3;
    }
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Swift
```swift
class Solution {
    func canThreePartsEqualSum(_ arr: [Int]) -> Bool {
        let totalSum = arr.reduce(0, +)
        if totalSum % 3 != 0 { return false }
        
        let target = totalSum / 3
        var currentSum = 0, count = 0
        
        for num in arr {
            currentSum += num
            if currentSum == target {
                count += 1
                currentSum = 0
            }
        }
        
        return count >= 3
    }
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Kotlin
```kotlin
class Solution {
    fun canThreePartsEqualSum(arr: IntArray): Boolean {
        val totalSum = arr.sum()
        if (totalSum % 3 != 0) return false
        
        val target = totalSum / 3
        var currentSum = 0
        var count = 0
        
        for (num in arr) {
            currentSum += num
            if (currentSum == target) {
                count++
                currentSum = 0
            }
        }
        
        return count >= 3
    }
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Dart
```dart
class Solution {
  bool canThreePartsEqualSum(List<int> arr) {
    int totalSum = arr.reduce((a, b) => a + b);
    if (totalSum % 3 != 0) return false;
    
    int target = totalSum ~/ 3, currentSum = 0, count = 0;
    for (int num in arr) {
      currentSum += num;
      if (currentSum == target) {
        count++;
        currentSum = 0;
      }
    }
    
    return count >= 3;
  }
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Go
```go
func canThreePartsEqualSum(arr []int) bool {
    totalSum := 0
    for _, num := range arr {
        totalSum += num
    }
    if totalSum % 3 != 0 {
        return false
    }
    
    target := totalSum / 3
    currentSum, count := 0, 0
    for _, num := range arr {
        currentSum += num
        if currentSum == target {
            count++
            currentSum = 0
        }
    }
    
    return count >= 3
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Ruby
```ruby
# @param {Integer[]} arr
# @return {Boolean}
def can_three_parts_equal_sum(arr)
    total_sum = arr.sum
    return false if total_sum % 3 != 0
    
    target, current_sum, count = total_sum / 3, 0, 0
    arr.each do |num|
        current_sum += num
        if current_sum == target
            count += 1
            current_sum = 0
        end
    end
    
    count >= 3
end
# Time Complexity: O(n)
# Space Complexity: O(1)
```

### Scala
```scala
object Solution {
    def canThreePartsEqualSum(arr: Array[Int]): Boolean = {
        val totalSum = arr.sum
        if (totalSum % 3 != 0) return false
        
        val target = totalSum / 3
        var currentSum = 0
        var count = 0
        
        for (num <- arr) {
            currentSum += num
            if (currentSum == target) {
                count += 1
                currentSum = 0
            }
        }
        
        count >= 3
    }
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Rust
```rust
impl Solution {
    pub fn can_three_parts_equal_sum(arr: Vec<i32>) -> bool {
        let total_sum: i32 = arr.iter().sum();
        if total_sum % 3 != 0 {
            return false;
        }
        
        let target = total_sum / 3;
        let mut current_sum = 0;
        let mut count = 0;
        
        for &num in &arr {
            current_sum += num;
            if current_sum == target {
                count += 1;
                current_sum = 0;
            }
        }
        
        count >= 3
    }
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Racket
```racket
(define/contract (can-three-parts-equal-sum arr)
  (-> (listof exact-integer?) boolean?)
  (let ([total-sum (apply + arr)])
    (if (not (= 0 (modulo total-sum 3)))
        #f
        (let loop ([lst arr]
                   [current-sum 0]
                   [count 0])
          (cond
           [(empty? lst) (>= count 3)]
           [else
            (define new-sum (+ current-sum (first lst)))
            (if (= new-sum (/ total-sum 3))
                (loop (rest lst) 0 (+ count 1))
                (loop (rest lst) new-sum count))])))))
;; Time Complexity: O(n)
;; Space Complexity: O(1)
```

### Erlang
```erlang
-spec can_three_parts_equal_sum(Arr :: [integer()]) -> boolean().
can_three_parts_equal_sum(Arr) ->
    TotalSum = lists:sum(Arr),
    if
        TotalSum rem 3 =/= 0 -> false;
        true ->
            Target = TotalSum div 3,
            {Count, _, _} = lists:foldl(
                fun (Num, {Count, CurrentSum, PrevCount}) ->
                    NewSum = CurrentSum + Num,
                    if
                        NewSum =:= Target -> 
                            {PrevCount + 1, 0, PrevCount + 1};
                        true -> 
                            {Count, NewSum, PrevCount}
                    end
                end, 
                {0, 0, 0}, 
                Arr
            ),
            Count >= 3
    end.
% Time Complexity: O(n)
% Space Complexity: O(1)
```

### Elixir
```elixir
defmodule Solution do
  @spec can_three_parts_equal_sum(arr :: [integer]) :: boolean
  def can_three_parts_equal_sum(arr) do
    total_sum = Enum.sum(arr)
    if rem(total_sum, 3) != 0 do
      false
    else
      target = div(total_sum, 3)
      count = 
        Enum.reduce_while(arr, {0, 0}, fn num, {count, current_sum} ->
          new_sum = current_sum + num
          if new_sum == target do
            if count + 1 == 3 do
              {:halt, true}
            else
              {:cont, {count + 1, 0}}
            end
          else
            {:cont, {count, new_sum}}
          end
        end)
      
      count >= 3
    end
  end
end
# Time Complexity: O(n)
# Space Complexity: O(1)
```

These implementations ensure that the problem is solved efficiently with a linear time complexity of O(n) and constant space complexity of O(1). Each method follows the discussed algorithm closely.


### Closing Statement

In this discussion, we tackled the problem of partitioning an array into three equal parts with equal sum by exploring both brute force and optimized approaches. We understood how the brute force method can be inefficient with O(n^3) complexity, and then we derived a more efficient solution using linear iteration, delivering an O(n) time complexity and O(1) space complexity.

We further implemented the optimized solution in multiple programming languages including C++, Java, Python, Python3, C, C#, JavaScript, TypeScript, PHP, Swift, Kotlin, Dart, Go, Ruby, Scala, Rust, Racket, Erlang, and Elixir. Each implementation kept the solution's time and space complexity at an optimal level.

Understanding such problems helps in improving skills related to array manipulation, partitioning problems, and algorithm optimization.

### Similar Questions

1. **Partition Equal Subset Sum**: Given a non-empty array of positive integers, determine if the array can be partitioned into two subsets such that the sum of elements in both subsets is equal.
   
2. **Subset Sum Problem**: Given a set of positive integers and a value sum, determine if there is a subset of the given set with sum equal to the given sum.
   
3. **Split Array with Equal Sum**: Given an array, determine if it can be divided into four contiguous subarrays such that the sums of elements in each subarray are all equal.
   
4. **Minimum Difference Subsets**: Partition the array into two subsets such that the absolute difference between their sums is minimized.

5. **Balanced Binary Tree Check**: Given a binary tree, determine if it's height-balanced, i.e., the depths of the left and right subtrees differ by no more than one.

These problems help deepen the understanding of partitioning and subset sum algorithms, and frequently appear in technical interviews, aiding in enhancing problem-solving skills significantly.