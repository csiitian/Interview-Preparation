### Interviewer and Interviewee Discussion

#### Interviewer:
So, we have a binary tree problem here. The goal is to find all root-to-leaf paths where the sum of node values equals a given `targetSum`. Could you talk me through your initial thoughts on how you might approach this problem?

#### Interviewee:
Sure. Initially, I think we can use a Depth-First Search (DFS) traversal to explore all possible root-to-leaf paths. While traversing, we can keep a running sum of node values and check if we reach the `targetSum` when we hit a leaf node.

#### Interviewer:
That sounds reasonable. Can you elaborate on your brute-force approach?

#### Interviewee:
Certainly. The brute-force approach involves:
1. Starting at the root node.
2. Using a DFS strategy to visit every possible path from the root to a leaf node.
3. Maintaining a list to store the nodes visited on the current path and a variable to store the sum of these nodes.
4. When a leaf node is reached, check if the sum equals `targetSum`. If it does, store the current path.
5. Backtrack to explore other possible paths.

#### Interviewer:
That's a good start. Could you discuss the time and space complexity of this brute-force approach?

#### Interviewee:
Sure. For time complexity, in the worst case, we will visit every node once. For each node, we might do some additional work to copy the path to the results list if it is a valid path. Let’s denote `N` as the number of nodes in the tree.
- **Time Complexity**: \( O(N^2) \). This is because we might traverse `N` nodes and for each leaf, we might have to copy a path of length `N` to the results list.

For space complexity:
- We need space for the call stack due to the recursive nature of DFS, which is \( O(H) \) where `H` is the height of the tree.
- Additionally, if all the paths are valid, we need space to store them. In the worst case, it can be \( O(L \cdot N) \) where `L` is the number of leaf nodes and each path can be of length `N`.
- **Space Complexity**: \( O(N) \) for the call stack and potentially \( O(N^2) \) for storing paths.

#### Interviewer:
Great! How can you optimize this approach so it works efficiently with larger inputs?

#### Interviewee:
We can still stick with DFS, but optimize it slightly:
1. Instead of copying the entire path, we can use a list to maintain the current path. This list can dynamically change as we explore different paths.
2. Use backtracking to efficiently add/remove nodes from the path.

Here's a more optimized approach using backtracking:

#### Optimized Approach

```python
class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

def pathSum(root, targetSum):
    def dfs(node, current_path, current_sum):
        if not node:
            return
        
        # Update current path and sum
        current_path.append(node.val)
        current_sum += node.val
        
        # Check if it's a leaf node and the path sum equals targetSum
        if not node.left and not node.right  and current_sum == targetSum:
            result.append(list(current_path))
            
        # Recurse down to children
        dfs(node.left, current_path, current_sum)
        dfs(node.right, current_path, current_sum)
        
        # Backtrack
        current_path.pop()

    result = []
    dfs(root, [], 0)
    return result
```

#### Time and Space Complexity

- **Time Complexity**: \( O(N) \) since we visit each node once.
- **Space Complexity**: \( O(H) \) for the recursive call stack, where `H` is the height of the tree. Additionally, the space required to store valid paths is still \( O(N) \) in the worst case.

### Drawing Explanation

Let's draw a simple tree to illustrate this:

```
       5
     /   \
    4     8
   /     / \
  11    13  4
 /  \        / \
7    2      5   1
```

For `targetSum = 22`, the paths are:

1. Starting at `5`, go to `4`, then to `11`, then to `7` or `2` to check:
   - `5 -> 4 -> 11 -> 2` equals `22`
   - `5 -> 4 -> 11 -> 7` does not.
2. Next path, starting at `5`, go to `8`:
   - `5 -> 8 -> 4 -> 5` equals `22`

### Conclusion
The optimized backtracking method helps efficiently traverse and find all root-to-leaf paths that sum to `targetSum`, utilizing space wisely and minimizing unnecessary path copies.
Sure, let's write the code for the given problem in all the provided languages. We'll make sure to include the function definitions and necessary structures, and we'll give special attention to the space and time complexities in the comments of each implementation.

#### C++
```cpp
/**
 * Definition for a binary tree node.
 * struct TreeNode {
 *     int val;
 *     TreeNode *left;
 *     TreeNode *right;
 *     TreeNode() : val(0), left(nullptr), right(nullptr) {}
 *     TreeNode(int x) : val(x), left(nullptr), right(nullptr) {}
 *     TreeNode(int x, TreeNode *left, TreeNode *right) : val(x), left(left), right(right) {}
 * };
 */
class Solution {
public:
    vector<vector<int>> pathSum(TreeNode* root, int targetSum) {
        vector<vector<int>> result;
        vector<int> currentPath;
        helper(root, targetSum, currentPath, result);
        return result;
    }
    
private:
    void helper(TreeNode* node, int targetSum, vector<int>& currentPath, vector<vector<int>>& result) {
        if (!node) return;
        
        currentPath.push_back(node->val);
        targetSum -= node->val;
        
        if (!node->left && !node->right && targetSum == 0) {
            result.push_back(currentPath);
        }
        
        helper(node->left, targetSum, currentPath, result);
        helper(node->right, targetSum, currentPath, result);
        
        currentPath.pop_back();
    }
};
// Time Complexity: O(N), N is the number of nodes
// Space Complexity: O(H), H is the height of the tree
```

#### Java
```java
/**
 * Definition for a binary tree node.
 * public class TreeNode {
 *     int val;
 *     TreeNode left;
 *     TreeNode right;
 *     TreeNode() {}
 *     TreeNode(int val) { this.val = val; }
 *     TreeNode(int val, TreeNode left, TreeNode right) {
 *         this.val = val;
 *         this.left = left;
 *         this.right = right;
 *     }
 * }
 */
class Solution {
    public List<List<Integer>> pathSum(TreeNode root, int targetSum) {
        List<List<Integer>> result = new ArrayList<>();
        List<Integer> currentPath = new ArrayList<>();
        helper(root, targetSum, currentPath, result);
        return result;
    }
    
    private void helper(TreeNode node, int targetSum, List<Integer> currentPath, List<List<Integer>> result) {
        if (node == null) return;
        
        currentPath.add(node.val);
        targetSum -= node.val;
        
        if (node.left == null && node.right == null && targetSum == 0) {
            result.add(new ArrayList<>(currentPath));
        }
        
        helper(node.left, targetSum, currentPath, result);
        helper(node.right, targetSum, currentPath, result);
        
        currentPath.remove(currentPath.size() - 1);
    }
}
// Time Complexity: O(N), N is the number of nodes
// Space Complexity: O(H), H is the height of the tree
```

#### Python
```python
# Definition for a binary tree node.
# class TreeNode(object):
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right

class Solution(object):
    def pathSum(self, root, targetSum):
        """
        :type root: TreeNode
        :type targetSum: int
        :rtype: List[List[int]]
        """
        def dfs(node, current_path, current_sum):
            if not node:
                return
            current_path.append(node.val)
            current_sum += node.val
            
            if not node.left and not node.right and current_sum == targetSum:
                result.append(list(current_path))
                
            dfs(node.left, current_path, current_sum)
            dfs(node.right, current_path, current_sum)
            
            current_path.pop()
        
        result = []
        dfs(root, [], 0)
        return result

# Time Complexity: O(N), N is the number of nodes
# Space Complexity: O(H), H is the height of the tree
```

#### Python3
```python
# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right

class Solution:
    def pathSum(self, root: Optional[TreeNode], targetSum: int) -> List[List[int]]:
        def dfs(node, current_path, current_sum):
            if not node:
                return
            current_path.append(node.val)
            current_sum += node.val
            
            if not node.left and not node.right and current_sum == targetSum:
                result.append(list(current_path))
                
            dfs(node.left, current_path, current_sum)
            dfs(node.right, current_path, current_sum)
            
            current_path.pop()
        
        result = []
        dfs(root, [], 0)
        return result

# Time Complexity: O(N), N is the number of nodes
# Space Complexity: O(H), H is the height of the tree
```

#### C
```c
/**
 * Definition for a binary tree node.
 * struct TreeNode {
 *     int val;
 *     struct TreeNode *left;
 *     struct TreeNode *right;
 * };
 */
/**
 * Return an array of arrays of size *returnSize.
 * The sizes of the arrays are returned as *returnColumnSizes array.
 * Note: Both returned array and *columnSizes array must be malloced, assume caller calls free().
 */
#include <stdlib.h>

void collectPaths(struct TreeNode* node, int targetSum, int* currentPath, int currentLength, int** result, int* resultSize, int*** resultColumnSizes) {
    if (!node) return;

    currentPath[currentLength++] = node->val;
    targetSum -= node->val;

    if (!node->left && !node->right && targetSum == 0) {
        result[*resultSize] = (int*)malloc(currentLength * sizeof(int));
        for (int i = 0; i < currentLength; i++) {
            result[*resultSize][i] = currentPath[i];
        }
        (*resultColumnSizes)[*resultSize] = (int*)malloc(sizeof(int));
        (*resultColumnSizes)[*resultSize][0] = currentLength;
        (*resultSize)++;
    }

    collectPaths(node->left, targetSum, currentPath, currentLength, result, resultSize, resultColumnSizes);
    collectPaths(node->right, targetSum, currentPath, currentLength, result, resultSize, resultColumnSizes);
}

int** pathSum(struct TreeNode* root, int targetSum, int* returnSize, int** returnColumnSizes) {
    int** result = (int**)malloc(1500 * sizeof(int*));
    *returnSize = 0;
    *returnColumnSizes = (int**)malloc(1500 * sizeof(int*));

    int* currentPath = (int*)malloc(1000 * sizeof(int)); // tree can have at most 1000 levels
    collectPaths(root, targetSum, currentPath, 0, result, returnSize, returnColumnSizes);

    free(currentPath);
    return result;
}

// Time Complexity: O(N), N is the number of nodes
// Space Complexity: O(H), H is the height of the tree
```

#### C#
```csharp
/**
 * Definition for a binary tree node.
 * public class TreeNode {
 *     public int val;
 *     public TreeNode left;
 *     public TreeNode right;
 *     public TreeNode(int val=0, TreeNode left=null, TreeNode right=null) {
 *         this.val = val;
 *         this.left = left;
 *         this.right = right;
 *     }
 * }
 */
public class Solution {
    public IList<IList<int>> PathSum(TreeNode root, int targetSum) {
        var result = new List<IList<int>>();
        var currentPath = new List<int>();
        Helper(root, targetSum, currentPath, result);
        return result;
    }
    
    private void Helper(TreeNode node, int targetSum, IList<int> currentPath, IList<IList<int>> result) {
        if (node == null) return;
        
        currentPath.Add(node.val);
        targetSum -= node.val;
        
        if (node.left == null && node.right == null && targetSum == 0) {
            result.Add(new List<int>(currentPath));
        }
        
        Helper(node.left, targetSum, currentPath, result);
        Helper(node.right, targetSum, currentPath, result);
        
        currentPath.RemoveAt(currentPath.Count - 1);
    }
}
// Time Complexity: O(N), N is the number of nodes
// Space Complexity: O(H), H is the height of the tree
```

#### JavaScript
```javascript
/**
 * Definition for a binary tree node.
 * function TreeNode(val, left, right) {
 *     this.val = (val===undefined ? 0 : val)
 *     this.left = (left===undefined ? null : left)
 *     this.right = (right===undefined ? null : right)
 * }
 */
/**
 * @param {TreeNode} root
 * @param {number} targetSum
 * @return {number[][]}
 */
var pathSum = function(root, targetSum) {
    let result = [];
    let currentPath = [];
    
    function dfs(node, currentSum) {
        if (!node) return;
        
        currentPath.push(node.val);
        currentSum += node.val;
        
        if (!node.left && !node.right && currentSum === targetSum) {
            result.push([...currentPath]);
        }
        
        dfs(node.left, currentSum);
        dfs(node.right, currentSum);
        
        currentPath.pop();
    }
    
    dfs(root, 0);
    return result;
};
// Time Complexity: O(N), N is the number of nodes
// Space Complexity: O(H), H is the height of the tree
```

#### TypeScript
```typescript
/**
 * Definition for a binary tree node.
 * class TreeNode {
 *     val: number
 *     left: TreeNode | null
 *     right: TreeNode | null
 *     constructor(val?: number, left?: TreeNode | null, right?: TreeNode | null) {
 *         this.val = (val===undefined ? 0 : val)
 *         this.left = (left===undefined ? null : left)
 *         this.right = (right===undefined ? null : right)
 *     }
 * }
 */

function pathSum(root: TreeNode | null, targetSum: number): number[][] {
    let result: number[][] = [];
    let currentPath: number[] = [];
    
    function dfs(node: TreeNode | null, currentSum: number): void {
        if (!node) return;
        
        currentPath.push(node.val);
        currentSum += node.val;
        
        if (!node.left && !node.right && currentSum === targetSum) {
            result.push([...currentPath]);
        }
        
        dfs(node.left, currentSum);
        dfs(node.right, currentSum);
        
        currentPath.pop();
    }
    
    dfs(root, 0);
    return result;
}
// Time Complexity: O(N), N is the number of nodes
// Space Complexity: O(H), H is the height of the tree
```

#### PHP
```php
/**
 * Definition for a binary tree node.
 * class TreeNode {
 *     public $val = null;
 *     public $left = null;
 *     public $right = null;
 *     function __construct($val = 0, $left = null, $right = null) {
 *         $this->val = $val;
 *         $this->left = $left;
 *         $this->right = $right;
 *     }
 * }
 */
class Solution {

    /**
     * @param TreeNode $root
     * @param Integer $targetSum
     * @return Integer[][]
     */
    function pathSum($root, $targetSum) {
        $result = [];
        $currentPath = [];
        $this->dfs($root, $targetSum, $currentPath, $result);
        return $result;
    }
    
    private function dfs($node, $targetSum, &$currentPath, &$result) {
        if (!$node) return;
        
        $currentPath[] = $node->val;
        $targetSum -= $node->val;
        
        if (!$node->left && !$node->right && $targetSum == 0) {
            $result[] = $currentPath;
        }
        
        $this->dfs($node->left, $targetSum, $currentPath, $result);
        $this->dfs($node->right, $targetSum, $currentPath, $result);
        
        array_pop($currentPath);
    }
}
// Time Complexity: O(N), N is the number of nodes
// Space Complexity: O(H), H is the height of the tree
```

#### Swift
```swift
/**
 * Definition for a binary tree node.
 * public class TreeNode {
 *     public var val: Int
 *     public var left: TreeNode?
 *     public var right: TreeNode?
 *     public init() { self.val = 0; self.left = nil; self.right = nil; }
 *     public init(_ val: Int) { self.val = val; self.left = nil; self.right = nil; }
 *     public init(_ val: Int, _ left: TreeNode?, _ right: TreeNode?) {
 *         self.val = val
 *         self.left = left
 *         self.right = right
 *     }
 * }
 */
class Solution {
    func pathSum(_ root: TreeNode?, _ targetSum: Int) -> [[Int]] {
        var result = [[Int]]()
        var currentPath = [Int]()
        
        func dfs(_ node: TreeNode?, _ currentSum: Int) {
            guard let node = node else { return }
            
            currentPath.append(node.val)
            let updatedSum = currentSum + node.val
            
            if node.left == nil && node.right == nil && updatedSum == targetSum {
                result.append(currentPath)
            }
            
            dfs(node.left, updatedSum)
            dfs(node.right, updatedSum)
            
            currentPath.removeLast()
        }
        
        dfs(root, 0)
        return result
    }
}
// Time Complexity: O(N), N is the number of nodes
// Space Complexity: O(H), H is the height of the tree
```

#### Kotlin
```kotlin
/**
 * Definition for a binary tree node.
 * class TreeNode(var val: Int) {
 *     var left: TreeNode? = null
 *     var right: TreeNode? = null
 * }
 */
class Solution {
    fun pathSum(root: TreeNode?, targetSum: Int): List<List<Int>> {
        val result = mutableListOf<List<Int>>()
        val currentPath = mutableListOf<Int>()
        
        fun dfs(node: TreeNode?, currentSum: Int) {
            if (node == null) return
            
            currentPath.add(node.val)
            val updatedSum = currentSum + node.val
            
            if (node.left == null && node.right == null && updatedSum == targetSum) {
                result.add(currentPath.toList())
            }
            
            dfs(node.left, updatedSum)
            dfs(node.right, updatedSum)
            
            currentPath.removeAt(currentPath.size - 1)
        }
        
        dfs(root, 0)
        return result
    }
}
// Time Complexity: O(N), N is the number of nodes
// Space Complexity: O(H), H is the height of the tree
```

#### Dart
```dart
/**
 * Definition for a binary tree node.
 * class TreeNode {
 *   int val;
 *   TreeNode? left;
 *   TreeNode? right;
 *   TreeNode([this.val = 0, this.left, this.right]);
 * }
 */

class Solution {
  List<List<int>> pathSum(TreeNode? root, int targetSum) {
    List<List<int>> result = [];
    List<int> currentPath = [];
    
    void dfs(TreeNode? node, int currentSum) {
      if (node == null) return;
      
      currentPath.add(node.val);
      currentSum += node.val;
      
      if (node.left == null && node.right == null && currentSum == targetSum) {
        result.add([...currentPath]);
      }
      
      dfs(node.left, currentSum);
      dfs(node.right, currentSum);
      
      currentPath.removeLast();
    }
    
    dfs(root, 0);
    return result;
  }
}
// Time Complexity: O(N), N is the number of nodes
// Space Complexity: O(H), H is the height of the tree
```

#### Go
```go
/**
 * Definition for a binary tree node.
 * type TreeNode struct {
 *     Val int
 *     Left *TreeNode
 *     Right *TreeNode
 * }
 */
func pathSum(root *TreeNode, targetSum int) [][]int {
    var result [][]int
    var currentPath []int
    
    var dfs func(node *TreeNode, currentSum int)
    dfs = func(node *TreeNode, currentSum int) {
        if node == nil {
            return
        }
        
        currentPath = append(currentPath, node.Val)
        currentSum += node.Val
        
        if node.Left == nil && node.Right == nil && currentSum == targetSum {
            path := make([]int, len(currentPath))
            copy(path, currentPath)
            result = append(result, path)
        }
        
        dfs(node.Left, currentSum)
        dfs(node.Right, currentSum)
        
        currentPath = currentPath[:len(currentPath)-1]
    }
    
    dfs(root, 0)
    return result
}
// Time Complexity: O(N), N is the number of nodes
// Space Complexity: O(H), H is the height of the tree
```


### Closing Statement

Great job! We have successfully tackled the problem of finding all root-to-leaf paths in a binary tree where the sum of the node values equals a given `targetSum`. We've implemented the solution in multiple programming languages, ensuring that our approach is both time and space efficient.

Our initial discussion on the brute-force method gave us a clear direction on how to approach the problem using Depth-First Search (DFS). We then optimized our solution by employing backtracking to dynamically manage the current path and minimize redundant operations.

We also discussed the time and space complexity of our solutions, ensuring our methods are optimal for larger inputs and adhere to the constraints. This structured approach can be extended to similar problems involving tree traversals and path calculations.

### Similar Questions

If you found this problem interesting, here are a few more similar questions you might want to try:

1. **Path Sum**:
   - _Description_: Given the `root` of a binary tree and an integer `targetSum`, return `true` if the tree has a root-to-leaf path such that adding up all the values along the path equals `targetSum`.
   - _Difficulty_: Easy
   - _Link_: [LeetCode 112](https://leetcode.com/problems/path-sum/)

2. **Path Sum III**:
   - _Description_: Given the `root` of a binary tree and an integer `targetSum`, return the number of paths that sum to `targetSum`. The path does not need to start or end at the root or a leaf, but it must go downwards.
   - _Difficulty_: Medium
   - _Link_: [LeetCode 437](https://leetcode.com/problems/path-sum-iii/)

3. **Binary Tree Paths**:
   - _Description_: Given the `root` of a binary tree, return all root-to-leaf paths in the form of strings.
   - _Difficulty_: Easy
   - _Link_: [LeetCode 257](https://leetcode.com/problems/binary-tree-paths/)

4. **Sum Root to Leaf Numbers**:
   - _Description_: Given a binary tree where each path from root to leaf represents a number, return the sum of all the numbers.
   - _Difficulty_: Medium
   - _Link_: [LeetCode 129](https://leetcode.com/problems/sum-root-to-leaf-numbers/)

5. **Minimum Path Sum**:
   - _Description_: Given an `m` x `n` grid filled with non-negative numbers, find a path from top-left to bottom-right that minimizes the sum of all numbers along its path.
   - _Difficulty_: Medium
   - _Link_: [LeetCode 64](https://leetcode.com/problems/minimum-path-sum/)

6. **Unique Paths**:
   - _Description_: A robot is located at the top-left corner of an `m` x `n` grid and can only move either down or right. Count the number of unique paths to reach the bottom-right corner.
   - _Difficulty_: Medium
   - _Link_: [LeetCode 62](https://leetcode.com/problems/unique-paths/)

By solving these related problems, you will improve your understanding of tree traversals, dynamic programming, and recursive algorithms, further building your problem-solving skills in various scenarios. Happy coding!