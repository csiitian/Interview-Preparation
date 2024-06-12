### Interviewer and Interviewee Discussion

**Interviewer:** Let's talk about the given problem. You are provided with a list of candidate numbers and a target number. Your task is to find all unique combinations in the candidates list where the candidate numbers sum to the target. Each candidate number can only be used once in the combination. What are your initial thoughts on how to approach this problem?

**Interviewee:** To start with, I would consider using a brute force approach to solve this problem. The idea would involve generating all possible subsets of the candidates list, then checking which of these subsets sum up to the target value. However, I believe this might not be efficient given the potential size of the candidates list.

**Interviewer:** That sounds like a reasonable first step. How would you describe the time and space complexity of this brute force approach?

**Interviewee:** In terms of complexity:
- **Time Complexity:** Generating all subsets of a list of size `n` involves `2^n` combinations. For each subset, summing up the elements takes `O(n)` time. Thus, the overall time complexity would be \( O(n \cdot 2^n) \).
- **Space Complexity:** We would need space to store all the subsets, which is also \( O(2^n) \).

**Interviewer:** Right. Given that the brute force method isn't efficient, can you think of a more optimized approach?

**Interviewee:** Yes, we can leverage backtracking to build the combinations recursively. By sorting the candidates array first and using a set to track unique combinations, we can avoid duplicates and make our solution more efficient.

### Optimized Approach

For the optimized approach, let's use backtracking, which is both time-efficient and space-efficient. Here is how we can structure our solution:

1. **Sort the candidates list:** This helps in both generating ordered combinations and skipping duplicates easily.
2. **Use a backtracking function:** It will recursively build combinations, keeping track of the current combination and the remaining target sum.
3. **Pruning the search space:** If at any point the remaining target becomes negative, we backtrack, as any further exploration won't yield a valid combination.

### Time and Space Complexity
- **Time Complexity:** The time complexity of our solution is \(O(2^n)\) in the worst case, as each candidate might be included or excluded. The sorting step adds an additional \(O(n \log n)\) complexity.
- **Space Complexity:** The space complexity is \(O(n)\), which accounts for the space used by the recursion stack and the temporary combination list.

### Illustrative Drawing

Imagine the candidates array is `[1, 1, 2, 5, 6, 7, 10]` and the target is `8`. Here's a conceptual drawing of the backtracking process:

1. Initially, the sorted candidates: `[1, 1, 2, 5, 6, 7, 10]`
2. Recursive calls will generate branches like:
   - Start with `1`, remaining target `7`
     - Further dive with `1` again, remaining target `6`
       - Continue with `2`, remaining target `4`
         - And so on, until all combinations starting with `1` are explored.
         - Then backtrack and start with the next candidate.

```
                      []
              /    |    \   .......
           [1]    [1]  [2]  
          /       |     \
      [1,1]   [1,2]  [2,3] 
      /  |    /  |    .....
  [1,1,2] ........ [2,6]
   /  | ......... /   |
 [1,1,2,5] .....[2,6,7]
```

Each path explored will be pruned if the remaining target goes negative, and duplicates are skipped efficiently due to the sorted list and conditional checks.

### Example Code

Here's a sample implementation of this approach in Python:

```python
def combinationSum2(candidates, target):
    def backtrack(start, path, rem_target):
        if rem_target == 0:
            results.append(path[:])
            return
        for i in range(start, len(candidates)):
            if i > start and candidates[i] == candidates[i - 1]:
                continue  # skip duplicates
            if candidates[i] > rem_target:
                break  # no need to continue if the candidate exceeds the remaining target
            path.append(candidates[i])
            backtrack(i + 1, path, rem_target - candidates[i])
            path.pop()  # backtrack

    candidates.sort()  # sort the candidates array
    results = []
    backtrack(0, [], target)
    return results

# Example usage:
candidates = [10, 1, 2, 7, 6, 1, 5]
target = 8
print(combinationSum2(candidates, target))
```

This code snippet utilizes the backtracking technique discussed, ensuring we generate all unique combinations efficiently.
Sure, let's implement the solution in each of the provided languages, while adding comments to mention the time and space complexity of the implementation.

### C++ Implementation
```cpp
class Solution {
public:
    vector<vector<int>> combinationSum2(vector<int>& candidates, int target) {
        sort(candidates.begin(), candidates.end());
        vector<vector<int>> results;
        vector<int> combination;
        backtrack(candidates, target, 0, combination, results);
        return results;
    }

private:
    void backtrack(vector<int>& candidates, int target, int start, vector<int>& combination, vector<vector<int>>& results) {
        if (target == 0) {
            results.push_back(combination);
            return;
        }
        for (int i = start; i < candidates.size() && target >= candidates[i]; ++i) {
            if (i > start && candidates[i] == candidates[i - 1]) continue; // skip duplicates
            combination.push_back(candidates[i]);
            backtrack(candidates, target - candidates[i], i + 1, combination, results);
            combination.pop_back();
        }
    }
};
/*
Time Complexity: O(2^n), where n is the number of candidates.
Space Complexity: O(n), where n is the depth of the recursion tree.
*/
```

### Java Implementation
```java
import java.util.*;

class Solution {
    public List<List<Integer>> combinationSum2(int[] candidates, int target) {
        Arrays.sort(candidates);
        List<List<Integer>> results = new ArrayList<>();
        backtrack(candidates, target, 0, new ArrayList<>(), results);
        return results;
    }

    private void backtrack(int[] candidates, int target, int start, List<Integer> combination, List<List<Integer>> results) {
        if (target == 0) {
            results.add(new ArrayList<>(combination));
            return;
        }
        for (int i = start; i < candidates.length && target >= candidates[i]; i++) {
            if (i > start && candidates[i] == candidates[i - 1]) continue; // skip duplicates
            combination.add(candidates[i]);
            backtrack(candidates, target - candidates[i], i + 1, combination, results);
            combination.remove(combination.size() - 1);
        }
    }
}
/*
Time Complexity: O(2^n), where n is the number of candidates.
Space Complexity: O(n), where n is the depth of the recursion tree.
*/
```

### Python Implementation
```python
class Solution(object):
    def combinationSum2(self, candidates, target):
        """
        :type candidates: List[int]
        :type target: int
        :rtype: List[List[int]]
        """
        candidates.sort()
        results = []
        self.backtrack(candidates, target, 0, [], results)
        return results

    def backtrack(self, candidates, target, start, combination, results):
        if target == 0:
            results.append(list(combination))
            return
        for i in range(start, len(candidates)):
            if i > start and candidates[i] == candidates[i - 1]:
                continue  # skip duplicates
            if candidates[i] > target:
                break
            combination.append(candidates[i])
            self.backtrack(candidates, target - candidates[i], i + 1, combination, results)
            combination.pop()

# Time Complexity: O(2^n), where n is the number of candidates.
# Space Complexity: O(n), where n is the depth of the recursion tree.
```

### Python 3 Implementation
```python
class Solution:
    def combinationSum2(self, candidates: List[int], target: int) -> List[List[int]]:
        candidates.sort()
        results = []
        self.backtrack(candidates, target, 0, [], results)
        return results

    def backtrack(self, candidates, target, start, combination, results):
        if target == 0:
            results.append(list(combination))
            return
        for i in range(start, len(candidates)):
            if i > start and candidates[i] == candidates[i - 1]:
                continue  # skip duplicates
            if candidates[i] > target:
                break
            combination.append(candidates[i])
            self.backtrack(candidates, target - candidates[i], i + 1, combination, results)
            combination.pop()

# Time Complexity: O(2^n), where n is the number of candidates.
# Space Complexity: O(n), where n is the depth of the recursion tree.
```

### C Implementation
```c
#include <stdlib.h>

/**
 * Return an array of arrays of size *returnSize.
 * The sizes of the arrays are returned as *returnColumnSizes array.
 * Note: Both returned array and *columnSizes array must be malloced, assume caller calls free().
 */
int** combinationSum2(int* candidates, int candidatesSize, int target, int* returnSize, int** returnColumnSizes) {
    int **results = malloc(500 * sizeof(int*));
    *returnColumnSizes = malloc(500 * sizeof(int));
    *returnSize = 0;

    void backtrack(int *candidates, int target, int start, int *combination, int combinationSize) {
        if (target == 0) {
            results[*returnSize] = malloc(combinationSize * sizeof(int));
            memcpy(results[*returnSize], combination, combinationSize * sizeof(int));
            (*returnColumnSizes)[*returnSize] = combinationSize;
            (*returnSize)++;
            return;
        }
        for (int i = start; i < candidatesSize && target >= candidates[i]; ++i) {
            if (i > start && candidates[i] == candidates[i - 1]) continue;
            combination[combinationSize] = candidates[i];
            backtrack(candidates, target - candidates[i], i + 1, combination, combinationSize + 1);
        }
    }

    qsort(candidates, candidatesSize, sizeof(int), (int(*)(const void*, const void*))strcmp);
    int combination[100];
    backtrack(candidates, target, 0, combination, 0);
    return results;
}
/*
Time Complexity: O(2^n), where n is the number of candidates.
Space Complexity: O(n), where n is the depth of the recursion tree.
*/
```

### C# Implementation
```csharp
using System;
using System.Collections.Generic;

public class Solution {
    public IList<IList<int>> CombinationSum2(int[] candidates, int target) {
        Array.Sort(candidates);
        IList<IList<int>> results = new List<IList<int>>();
        Backtrack(candidates, target, 0, new List<int>(), results);
        return results;
    }

    private void Backtrack(int[] candidates, int target, int start, IList<int> combination, IList<IList<int>> results) {
        if (target == 0) {
            results.Add(new List<int>(combination));
            return;
        }
        for (int i = start; i < candidates.Length && target >= candidates[i]; i++) {
            if (i > start && candidates[i] == candidates[i - 1]) continue; // skip duplicates
            combination.Add(candidates[i]);
            Backtrack(candidates, target - candidates[i], i + 1, combination, results);
            combination.RemoveAt(combination.Count - 1);
        }
    }
}
/*
Time Complexity: O(2^n), where n is the number of candidates.
Space Complexity: O(n), where n is the depth of the recursion tree.
*/
```

### JavaScript Implementation
```javascript
/**
 * @param {number[]} candidates
 * @param {number} target
 * @return {number[][]}
 */
var combinationSum2 = function(candidates, target) {
    candidates.sort((a, b) => a - b);
    const results = [];
    const backtrack = (start, comb, remainingTarget) => {
        if (remainingTarget === 0) {
            results.push([...comb]);
            return;
        }
        for (let i = start; i < candidates.length; i++) {
            if (i > start && candidates[i] === candidates[i - 1]) continue; // skip duplicates
            if (candidates[i] > remainingTarget) break;
            comb.push(candidates[i]);
            backtrack(i + 1, comb, remainingTarget - candidates[i]);
            comb.pop();
        }
    };

    backtrack(0, [], target);
    return results;
};
/*
Time Complexity: O(2^n), where n is the number of candidates.
Space Complexity: O(n), where n is the depth of the recursion tree.
*/
```

### TypeScript Implementation
```typescript
function combinationSum2(candidates: number[], target: number): number[][] {
    candidates.sort((a, b) => a - b);
    const results: number[][] = [];
    const backtrack = (start: number, comb: number[], remainingTarget: number) => {
        if (remainingTarget === 0) {
            results.push([...comb]);
            return;
        }
        for (let i = start; i < candidates.length; i++) {
            if (i > start && candidates[i] === candidates[i - 1]) continue; // skip duplicates
            if (candidates[i] > remainingTarget) break;
            comb.push(candidates[i]);
            backtrack(i + 1, comb, remainingTarget - candidates[i]);
            comb.pop();
        }
    };

    backtrack(0, [], target);
    return results;
}
/*
Time Complexity: O(2^n), where n is the number of candidates.
Space Complexity: O(n), where n is the depth of the recursion tree.
*/
```

### PHP Implementation
```php
class Solution {
    /**
     * @param Integer[] $candidates
     * @param Integer $target
     * @return Integer[][]
     */
    function combinationSum2($candidates, $target) {
        sort($candidates);
        $results = [];
        $this->backtrack($candidates, $target, 0, [], $results);
        return $results;
    }

    private function backtrack($candidates, $target, $start, $combination, &$results) {
        if ($target == 0) {
            array_push($results, $combination);
            return;
        }
        for ($i = $start; $i < count($candidates); $i++) {
            if ($i > $start && $candidates[$i] == $candidates[$i - 1]) continue; // skip duplicates
            if ($candidates[$i] > $target) break;
            array_push($combination, $candidates[$i]);
            $this->backtrack($candidates, $target - $candidates[$i], $i + 1, $combination, $results);
            array_pop($combination);
        }
    }
}
/*
Time Complexity: O(2^n), where n is the number of candidates.
Space Complexity: O(n), where n is the depth of the recursion tree.
*/
```

### Swift Implementation
```swift
class Solution {
    func combinationSum2(_ candidates: [Int], _ target: Int) -> [[Int]] {
        let sortedCandidates = candidates.sorted()
        var results = [[Int]]()
        var combination = [Int]()
        backtrack(sortedCandidates, target, 0, &combination, &results)
        return results
    }

    private func backtrack(_ candidates: [Int], _ target: Int, _ start: Int, _ combination: inout [Int], _ results: inout [[Int]]) {
        if target == 0 {
            results.append(combination)
            return
        }
        for i in start..<candidates.count {
            if i > start && candidates[i] == candidates[i - 1] { continue } // skip duplicates
            if candidates[i] > target { break }
            combination.append(candidates[i])
            backtrack(candidates, target - candidates[i], i + 1, &combination, &results)
            combination.removeLast()
        }
    }
}
/*
Time Complexity: O(2^n), where n is the number of candidates.
Space Complexity: O(n), where n is the depth of the recursion tree.
*/
```

### Kotlin Implementation
```kotlin
class Solution {
    fun combinationSum2(candidates: IntArray, target: Int): List<List<Int>> {
        candidates.sort()
        val results = mutableListOf<List<Int>>()
        val combination = mutableListOf<Int>()
        backtrack(candidates, target, 0, combination, results)
        return results
    }

    private fun backtrack(candidates: IntArray, target: Int, start: Int, combination: MutableList<Int>, results: MutableList<List<Int>>) {
        if (target == 0) {
            results.add(ArrayList(combination))
            return
        }
        for (i in start until candidates.size) {
            if (i > start && candidates[i] == candidates[i - 1]) continue // skip duplicates
            if (candidates[i] > target) break
            combination.add(candidates[i])
            backtrack(candidates, target - candidates[i], i + 1, combination, results)
            combination.removeAt(combination.size - 1)
        }
    }
}
/*
Time Complexity: O(2^n), where n is the number of candidates.
Space Complexity: O(n), where n is the depth of the recursion tree.
*/
```

### Dart Implementation
```dart
class Solution {
  List<List<int>> combinationSum2(List<int> candidates, int target) {
    candidates.sort();
    List<List<int>> results = [];
    void backtrack(int start, List<int> combination, int remainingTarget) {
      if (remainingTarget == 0) {
        results.add(List.from(combination));
        return;
      }
      for (int i = start; i < candidates.length; i++) {
        if (i > start && candidates[i] == candidates[i - 1]) continue; // skip duplicates
        if (candidates[i] > remainingTarget) break;
        combination.add(candidates[i]);
        backtrack(i + 1, combination, remainingTarget - candidates[i]);
        combination.removeLast();
      }
    }

    backtrack(0, [], target);
    return results;
  }
}

/*
Time Complexity: O(2^n), where n is the number of candidates.
Space Complexity: O(n), where n is the depth of the recursion tree.
*/
```

### Go Implementation
```go
func combinationSum2(candidates []int, target int) [][]int {
    sort.Ints(candidates)
    var results [][]int
    var combination []int

    var backtrack func(int, int)
    backtrack = func(start, remain int) {
        if remain == 0 {
            results = append(results, append([]int(nil), combination...))
            return
        }
        for i := start; i < len(candidates) && remain >= candidates[i]; i++ {
            if i > start && candidates[i] == candidates[i-1] {
                continue
            }
            combination = append(combination, candidates[i])
            backtrack(i+1, remain-candidates[i])
            combination = combination[:len(combination)-1]
        }
    }

    backtrack(0, target)
    return results
}

/*
Time Complexity: O(2^n), where n is the number of candidates.
Space Complexity: O(n), where n is the depth of the recursion tree.
*/
```

### Ruby Implementation
```ruby
# @param {Integer[]} candidates
# @param {Integer} target
# @return {Integer[][]}
def combination_sum2(candidates, target)
    candidates.sort!
    results = []
    backtrack(candidates, target, 0, [], results)
    results
end

def backtrack(candidates, target, start, combination, results)
    if target == 0
        results << combination.clone
        return
    end
    (start...candidates.size).each do |i|
        next if i > start && candidates[i] == candidates[i - 1]
        break if candidates[i] > target
        combination << candidates[i]
        backtrack(candidates, target - candidates[i], i + 1, combination, results)
        combination.pop
    end
end

# Time Complexity: O(2^n), where n is the number of candidates.
# Space Complexity: O(n), where n is the depth of the recursion tree.
```

### Scala Implementation
```scala
object Solution {
    def combinationSum2(candidates: Array[Int], target: Int): List[List[Int]] = {
        val sortedCandidates = candidates.sorted
        var results = List[List[Int]]()

        def backtrack(start: Int, comb: List[Int], remainingTarget: Int): Unit = {
            if (remainingTarget == 0) {
                results = results :+ comb
                return
            }
            for (i <- start until sortedCandidates.length if remainingTarget >= sortedCandidates(i)) {
                if (i > start && sortedCandidates(i) == sortedCandidates(i - 1)) {
                    // skip duplicates
                } else {
                    backtrack(i + 1, comb :+ sortedCandidates(i), remainingTarget - sortedCandidates(i))
                }
            }
        }

        backtrack(0, List[Int](), target)
        results
    }
}
/*
Time Complexity: O(2^n), where n is the number of candidates.
Space Complexity: O(n), where n is the depth of the recursion tree.
*/
```

### Rust Implementation
```rust
impl Solution {
    pub fn combination_sum2(candidates: Vec<i32>, target: i32) -> Vec<Vec<i32>> {
        let mut sorted_candidates = candidates;
        sorted_candidates.sort();
        let mut results = Vec::new();
        let mut combination = Vec::new();

        fn backtrack(
            candidates: &Vec<i32>,
            target: i32,
            start: usize,
            combination: &mut Vec<i32>,
            results: &mut Vec<Vec<i32>>,
        ) {
            if target == 0 {
                results.push(combination.clone());
                return;
            }
            for i in start..candidates.len() {
                if i > start && candidates[i] == candidates[i - 1] {
                    continue;
                }
                if candidates[i] > target {
                    break;
                }
                combination.push(candidates[i]);
                backtrack(candidates, target - candidates[i], i + 1, combination, results);
                combination.pop();
            }
        }

        backtrack(&sorted_candidates, target, 0, &mut combination, &mut results);
        results
    }
}

/*
Time Complexity: O(2^n), where n is the number of candidates.
Space Complexity: O(n), where n is the depth of the recursion tree.
*/
```


### Closing Statement:

We've successfully discussed and implemented the combination sum problem where each candidate number can only be used once in the combination. Starting from an initial brute force approach, we optimized the solution using backtracking to efficiently handle the constraints and avoid duplicate combinations. The solution leverages recursion and sorting to ensure all unique combinations are considered. We've also explored the implementation in various programming languages, ensuring our approach is versatile and broadly applicable.

This problem is a great example of how backtracking can effectively solve combinatorial problems by exploring all potential solutions in a structured manner. The additional complexity introduced by the need to avoid duplicates demonstrates the importance of vigilant handling of sorted input and the control of recursion in a detailed, step-by-step process.

### Similar Questions:

1. **Combination Sum**:
   - Find all unique combinations in candidates where the candidate numbers sum to a given target. Each number in candidates may be used an unlimited number of times.

2. **Permutations**:
   - Given an array of distinct integers, return all possible permutations.

3. **Subsets**:
   - Given an integer array of unique elements, return all possible subsets (the power set).

4. **Subset Sum**:
   - Given a set of integers and a target value, find if there is a subset whose sum is equal to the target value.

5. **Partition Equal Subset Sum**:
   - Given a non-empty array containing only positive numbers, find if the array can be partitioned into two subsets such that the sum of elements in both subsets is equal.

6. **Generate Parentheses**:
   - Given n pairs of parentheses, write a function to generate all combinations of well-formed parentheses.

7. **Palindrome Partitioning**:
   - Given a string s, partition s such that every substring of the partition is a palindrome. Return all possible palindrome partitioning of s.

8. **Letter Combinations of a Phone Number**:
   - Given a string containing digits from 2-9 inclusive, return all possible letter combinations that the number could represent.

These questions often involve similar techniques such as backtracking, recursion, and dynamic programming, making them excellent exercises to further hone problem-solving skills in combinatorial contexts.