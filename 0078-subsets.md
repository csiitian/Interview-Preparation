### Interviewer and Interviewee Discussion

**Interviewer:** Let's discuss the problem of finding all possible subsets of a given integer array `nums` with unique elements. Essentially, you need to return the power set of the array. How would you approach this problem?

**Interviewee:** Sure, let's start by understanding the problem better. Given an array `nums`, we need to generate all possible subsets, including the empty subset and the subset containing all elements. The solution should not have any duplicate subsets, which is guaranteed by the input constraint that all elements are unique. For example, for the array `[1,2,3]`, the output should be `[[], [1], [2], [1,2], [3], [1,3], [2,3], [1,2,3]]`.

**Interviewer:** That's right. Can you think of a brute force approach to solve this problem?

**Interviewee:** Yes, one brute force way to generate all subsets is to use recursion to include or exclude each element and generate all possible combinations. Here's how we can think about it:
1. Each element can either be included in a subset or not.
2. This decision process can be applied recursively to each element.

For an array of `n` elements, there are `2^n` possible subsets. So we can generate all subsets by choosing, for each element, whether to include it in the current subset or not.

**Interviewer:** That's a good start. What will be the time and space complexity of this brute force approach?

**Interviewee:** The time complexity of generating all `2^n` subsets is `O(2^n)` because each element has 2 choices (either to be included or excluded from a subset) and there are `n` elements. The space complexity is also `O(2^n)`, to store all subsets.

**Interviewer:** Right. Now, can you think of how we might optimize this approach, perhaps by using a different data structure or by leveraging iterative methods?

**Interviewee:** Absolutely. We can use an iterative approach to generate the power set which could be more straightforward and efficient in terms of implementation. We can start with an empty subset and iteratively add each element to the existing subsets.

Here's a step-by-step breakdown of this iterative method:
1. Start with an empty subset `[]`.
2. For each element in `nums`, take all existing subsets, and for each of them, create a new subset by adding the current element to it.
3. Add these new subsets to the list of all subsets.

Let's illustrate this with an example `nums = [1, 2, 3]`:
- Start with `[[]]`.
- For element `1`: add `1` to all existing subsets `[[]]` to get `[[], [1]]`.
- For element `2`: add `2` to all existing subsets `[[], [1]]` to get `[[], [1], [2], [1, 2]]`.
- For element `3`: add `3` to all existing subsets `[[], [1], [2], [1, 2]]` to get `[[], [1], [2], [1, 2], [3], [1, 3], [2, 3], [1, 2, 3]]`.

**Interviewer:** That sounds like a solid approach. What would be the time and space complexity for this iterative method?

**Interviewee:** The time complexity remains `O(2^n)` since we are still generating all possible subsets. However, the iterative approach can be more space-efficient in practice as it avoids the overhead of recursive calls and stack space. The space complexity is again `O(2^n)` to store all subsets.

**Interviewer:** Great. Can you draw an illustration to demonstrate the iterative process?

**Interviewee:** Sure, here's a visual representation of the iterative approach with `nums = [1, 2, 3]`:

**Iterative Process Illustration:**

```
Initial: [[]]

After processing 1:
[[],
 [1]]

After processing 2:
[[],
 [1],
 [2],
 [1, 2]]

After processing 3:
[[],
 [1],
 [2],
 [1, 2],
 [3],
 [1, 3],
 [2, 3],
 [1, 2, 3]]
```

**Interviewer:** Nicely done. This covers all possible subsets iteratively. Let's implement the function in Python to complete our discussion.

**Interviewee:** Sure, here's a simple implementation in Python:

```python
def subsets(nums):
    result = [[]]
    for num in nums:
        new_subsets = [curr + [num] for curr in result]
        result.extend(new_subsets)
    return result

# Example usage
nums = [1, 2, 3]
print(subsets(nums))
```

**Interviewer:** Excellent. This implementation seems efficient and clear. Well done!
Sure, let's start by implementing the code for generating all subsets iteratively in each of the provided languages. I'll include the time and space complexity comments as well.

### C++
```cpp
class Solution {
public:
    vector<vector<int>> subsets(vector<int>& nums) {
        vector<vector<int>> result = {{}};
        for (int num : nums) {
            int n = result.size();
            for (int i = 0; i < n; ++i) {
                vector<int> subset = result[i];
                subset.push_back(num);
                result.push_back(subset);
            }
        }
        return result;
    }
};
// Time Complexity: O(2^n)
// Space Complexity: O(2^n)
```

### Java
```java
class Solution {
    public List<List<Integer>> subsets(int[] nums) {
        List<List<Integer>> result = new ArrayList<>();
        result.add(new ArrayList<>());
        for (int num : nums) {
            int n = result.size();
            for (int i = 0; i < n; ++i) {
                List<Integer> subset = new ArrayList<>(result.get(i));
                subset.add(num);
                result.add(subset);
            }
        }
        return result;
    }
}
// Time Complexity: O(2^n)
// Space Complexity: O(2^n)
```

### Python
```python
class Solution(object):
    def subsets(self, nums):
        """
        :type nums: List[int]
        :rtype: List[List[int]]
        """
        result = [[]]
        for num in nums:
            new_subsets = [curr + [num] for curr in result]
            result.extend(new_subsets)
        return result
# Time Complexity: O(2^n)
# Space Complexity: O(2^n)
```

### Python3
```python
class Solution:
    def subsets(self, nums: List[int]) -> List[List[int]]:
        result = [[]]
        for num in nums:
            new_subsets = [curr + [num] for curr in result]
            result.extend(new_subsets)
        return result
# Time Complexity: O(2^n)
# Space Complexity: O(2^n)
```

### C
```c
#include <stdlib.h>

/**
 * Return an array of arrays of size *returnSize.
 * The sizes of the arrays are returned as *returnColumnSizes array.
 * Note: Both returned array and *columnSizes array must be malloced, assume caller calls free().
 */
int** subsets(int* nums, int numsSize, int* returnSize, int** returnColumnSizes) {
    int maxSize = 1 << numsSize; // 2^numsSize
    *returnSize = maxSize;
    *returnColumnSizes = (int*)malloc(maxSize * sizeof(int));

    int** result = (int**)malloc(maxSize * sizeof(int*));
    for (int i = 0; i < maxSize; ++i) {
        result[i] = (int*)malloc(numsSize * sizeof(int));
        (*returnColumnSizes)[i] = 0;
        for (int j = 0; j < numsSize; ++j) {
            if ((i & (1 << j)) != 0) {
                result[i][(*returnColumnSizes)[i]++] = nums[j];
            }
        }
    }
    return result;
}
// Time Complexity: O(2^n)
// Space Complexity: O(2^n)
```

### C#
```csharp
public class Solution {
    public IList<IList<int>> Subsets(int[] nums) {
        var result = new List<IList<int>>();
        result.Add(new List<int>());
        foreach (var num in nums) {
            var newSubsets = new List<IList<int>>();
            foreach (var subset in result) {
                var newSubset = new List<int>(subset) { num };
                newSubsets.Add(newSubset);
            }
            result.AddRange(newSubsets);
        }
        return result;
    }
}
// Time Complexity: O(2^n)
// Space Complexity: O(2^n)
```

### JavaScript
```javascript
/**
 * @param {number[]} nums
 * @return {number[][]}
 */
var subsets = function(nums) {
    const result = [[]];
    for (const num of nums) {
        const newSubsets = [];
        for (const curr of result) {
            newSubsets.push([...curr, num]);
        }
        result.push(...newSubsets);
    }
    return result;
};
// Time Complexity: O(2^n)
// Space Complexity: O(2^n)
```

### TypeScript
```typescript
function subsets(nums: number[]): number[][] {
    const result: number[][] = [[]];
    for (const num of nums) {
        const newSubsets = [];
        for (const curr of result) {
            newSubsets.push([...curr, num]);
        }
        result.push(...newSubsets);
    }
    return result;
};
// Time Complexity: O(2^n)
// Space Complexity: O(2^n)
```

### PHP
```php
class Solution {

    /**
     * @param Integer[] $nums
     * @return Integer[][]
     */
    function subsets($nums) {
        $result = [[]];
        foreach ($nums as $num) {
            $newSubsets = [];
            foreach ($result as $subset) {
                $newSubset = array_merge($subset, [$num]);
                $newSubsets[] = $newSubset;
            }
            $result = array_merge($result, $newSubsets);
        }
        return $result;
    }
}
// Time Complexity: O(2^n)
// Space Complexity: O(2^n)
```

### Swift
```swift
class Solution {
    func subsets(_ nums: [Int]) -> [[Int]] {
        var result = [[Int]]()
        result.append([])
        for num in nums {
            let newSubsets = result.map { $0 + [num] }
            result.append(contentsOf: newSubsets)
        }
        return result
    }
}
// Time Complexity: O(2^n)
// Space Complexity: O(2^n)
```

### Kotlin
```kotlin
class Solution {
    fun subsets(nums: IntArray): List<List<Int>> {
        val result = mutableListOf<List<Int>>()
        result.add(emptyList())
        for (num in nums) {
            val newSubsets = result.map { it + num }
            result.addAll(newSubsets)
        }
        return result
    }
}
// Time Complexity: O(2^n)
// Space Complexity: O(2^n)
```

### Dart
```dart
class Solution {
  List<List<int>> subsets(List<int> nums) {
    List<List<int>> result = [[]];
    for (int num in nums) {
      List<List<int>> newSubsets = [];
      for (List<int> curr in result) {
        List<int> newSubset = List.from(curr)..add(num);
        newSubsets.add(newSubset);
      }
      result.addAll(newSubsets);
    }
    return result;
  }
}
// Time Complexity: O(2^n)
// Space Complexity: O(2^n)
```

### Go
```go
func subsets(nums []int) [][]int {
    result := [][]int{{}}
    for _, num := range nums {
        newSubsets := [][]int{}
        for _, curr := range result {
            newSubset := append([]int(nil), curr...)
            newSubset = append(newSubset, num)
            newSubsets = append(newSubsets, newSubset)
        }
        result = append(result, newSubsets...)
    }
    return result
}
// Time Complexity: O(2^n)
// Space Complexity: O(2^n)
```

### Ruby
```ruby
# @param {Integer[]} nums
# @return {Integer[][]}
def subsets(nums)
    result = [[]]
    nums.each do |num|
        new_subsets = []
        result.each do |curr|
            new_subsets << curr + [num]
        end
        result.concat(new_subsets)
    end
    result
end
# Time Complexity: O(2^n)
# Space Complexity: O(2^n)
```

### Scala
```scala
object Solution {
    def subsets(nums: Array[Int]): List[List[Int]] = {
        var result: List[List[Int]] = List(List())
        for (num <- nums) {
            val newSubsets = result.map(curr => curr :+ num)
            result ++= newSubsets
        }
        result
    }
}
// Time Complexity: O(2^n)
// Space Complexity: O(2^n)
```

### Rust
```rust
impl Solution {
    pub fn subsets(nums: Vec<i32>) -> Vec<Vec<i32>> {
        let mut result = vec![vec![]];
        for num in nums {
            let new_subsets: Vec<Vec<i32>> = result.iter()
                .cloned()
                .map(|mut subset| { subset.push(num); subset })
                .collect();
            result.extend(new_subsets);
        }
        result
    }
}
// Time Complexity: O(2^n)
// Space Complexity: O(2^n)
```

### Racket
```racket
(define/contract (subsets nums)
  (-> (listof exact-integer?) (listof (listof exact-integer?)))
  (let loop ([nums nums] [result '(()])]
    (if (null? nums)
        (reverse result)
        (loop (cdr nums)
              (append result
                      (map (lambda (x) (cons (car nums) x))
                           result))))))
;; Time Complexity: O(2^n)
;; Space Complexity: O(2^n)
```

### Erlang
```erlang
-spec subsets(Nums :: [integer()]) -> [[integer()]].
subsets(Nums) ->
    subsets(Nums, [[]]).

subsets([], Result) ->
    Result;
subsets([Num | Rest], Result) ->
    NewSubsets = [Subset ++ [Num] || Subset <- Result],
    subsets(Rest, Result ++ NewSubsets).
% Time Complexity: O(2^n)
% Space Complexity: O(2^n)
```

### Elixir
```elixir
defmodule Solution do
  @spec subsets(nums :: [integer]) :: [[integer]]
  def subsets(nums) do
    Enum.reduce(nums, [[]], fn num, acc ->
      acc ++ Enum.map(acc, fn curr -> curr ++ [num] end)
    end)
  end
end
# Time Complexity: O(2^n)
# Space Complexity: O(2^n)
```

These implementations should meet the requirements for generating all possible subsets of a given list of unique integers in each respective language.


### Closing Statement

**Interviewer:** Great job! We've gone through the problem of generating all possible subsets of a given array of unique integers and explored both a brute-force recursive approach and a more optimized iterative method. We've also implemented the iterative approach in various programming languages, ensuring they all have efficient time and space complexities suitable for the problem constraints. You demonstrated a clear understanding of generating power sets and applied the concept effectively across different languages.

**Interviewee:** Thank you! It was a valuable exercise to see how the same logic translates across various languages, and it's fascinating to observe the similarities and differences in their syntaxes and approaches.

**Interviewer:** Absolutely. Understanding the fundamental concepts and being able to apply them across multiple languages is a critical skill in solving data structure and algorithm problems. Well done!

### Similar Questions

Here are some similar problems you could practice to strengthen your understanding of subsets, combinations, and permutations:

1. **Combinations (LeetCode #77)**
   - Given two integers `n` and `k`, return all possible combinations of `k` numbers out of `1 ... n`.

2. **Permutations (LeetCode #46)**
   - Given a collection of distinct integers, return all possible permutations.

3. **Combination Sum (LeetCode #39)**
   - Given an array of distinct integers `candidates` and a target integer `target`, return all unique combinations of `candidates` where the chosen numbers sum to `target`.

4. **Subsets II (LeetCode #90)**
   - Given an integer array `nums` that may contain duplicates, return all possible subsets (the power set). The solution set must not contain duplicate subsets.

5. **Letter Combinations of a Phone Number (LeetCode #17)**
   - Given a string containing digits from `2-9` inclusive, return all possible letter combinations that the number could represent.

6. **Generate Parentheses (LeetCode #22)**
   - Given `n` pairs of parentheses, write a function to generate all combinations of well-formed parentheses.

7. **Palindrome Partitioning (LeetCode #131)**
   - Given a string `s`, partition `s` such that every substring of the partition is a palindrome. Return all possible palindrome partitioning of `s`.

Practicing these problems will help you gain more confidence in generating combinations, handling unique constraints, and leveraging recursive and iterative techniques efficiently. Good luck with your continued learning!