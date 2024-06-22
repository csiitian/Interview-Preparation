### Interview Discussion

**Interviewer:** Suppose we have a binary tree. Each node in the tree holds an integer value. The idea is to find the maximum path sum. A path is defined as any sequence of nodes from some starting node to any node in the tree along the parent-child connections. The path must only be traversed in one direction, either from parent to child or vice versa, on each traversal. How would you approach this problem?

**Interviewee:** To clarify, a node can be part of different paths but can appear in a specific path only once, right?

**Interviewer:** Correct. And the path sum is the sum of node values in that path. Do you have any initial thoughts on how you might solve this?

**Interviewee:** Yes. My initial thought is to use a brute force approach where we consider every possible path in the tree and calculate its sum, then keep track of the maximum sum encountered.

**Interviewer:** That sounds like a good start. Can you elaborate on how this brute force approach works and its potential inefficiencies?

### Brute Force Approach

**Interviewee:** Sure. The brute force solution can be thought of in the following way:
1. For each node in the tree, consider it as a starting point.
2. From this node, explore every possible path that can emanate from it.
3. Calculate the sum of each path.
4. Keep track of the maximum path sum encountered.

**Interviewer:** That sounds like a comprehensive way to capture all possible paths. But how do you see the time and space complexity of this approach?

**Interviewee:** 
- **Time Complexity:** In the worst case, if the tree has `n` nodes, each path from every node could take O(n) time. Since we need to check from each node, it leads to a complexity of O(n^2) overall.
- **Space Complexity:** Given that we might need a call stack depth equivalent to the height of the tree (which is O(n) in the worst case for highly unbalanced trees), it could also take up to O(n) space for the call stack.

**Interviewer:** Good observation. This quadratic complexity might be infeasible for large trees. Can you think of a way to optimize it?

### Optimized Approach

**Interviewee:** To improve the efficiency, we can use dynamic programming with a depth-first search (DFS). Instead of recalculating the sums for every possible path starting from each node, we can compute the maximum path sum involving each node only once and propagate this information upward.

**Interviewer:** Interesting! How would you go about implementing this?

**Interviewee:**

1. **DFS Traversal:** Perform a DFS traversal on the tree.
2. **Path Sum Calculation:** For each node, calculate two values:
   - The maximum path sum **starting** from this node and heading downwards.
   - The maximum path sum **through** this node, which includes the node itself and both left and right subtrees.
3. **Update Maximum:** Keep track of the global maximum path sum encountered during the traversal.

### Detailed Implementation:

- **Helper Function:** A helper function returns the maximum path sum starting from the given node.
- **Path Sum Calculation:** Inside the helper function:
  - Recurse on left and right children to get their respective maximum sums.
  - Calculate the maximum single-path sum starting from the current node.
  - Calculate the path sum through the current node.
  - Update the global maximum.

### Pseudo-Implementation:

```python
class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

class Solution:
    def maxPathSum(self, root: TreeNode) -> int:
        self.global_max = float('-inf')

        def helper(node):
            if not node:
                return 0
            
            # Recursively get the max sum of left and right subtrees
            left_max = max(helper(node.left), 0)
            right_max = max(helper(node.right), 0)
            
            # Calculate the price of the current node being the root of the maximum path
            current_max = node.val + left_max + right_max

            # Update the global maximum if current maximum path sum is greater
            self.global_max = max(self.global_max, current_max)

            # Return the maximum path sum starting from the current node
            return node.val + max(left_max, right_max)

        helper(root)
        return self.global_max
```

### Complexity Analysis:

- **Time Complexity:** O(n), as each node is visited once.
- **Space Complexity:** O(h) where `h` is the height of the tree, due to the recursion stack. For balanced trees, this would be O(log n); for unbalanced trees, it could be O(n).

### Visual Explanation

Here's an example diagram that depicts the calculation steps:

```
Example Tree:
        -10
       /   \
      9     20
           /  \
          15   7

1. Calculate from the leaf nodes:
    - Node 15: Max single path = 15
    - Node 7: Max single path = 7

2. Combine at Node 20:
    - Combine left (15) and right (7): Max path through 20 = 15 + 20 + 7 = 42
    - Max single path starting from 20: max(15, 7) + 20 = 35

3. Combine at Root (-10):
    - Combine left (9) and right (35): Max path through -10 = 9 + (-10) + 35 = 34
    - Max single path starting from -10: max(9, 35) + (-10) = 25

Global maximum path sum during this traversal is 42 (from 15 -> 20 -> 7).
```

This optimized approach ensures that we leverage the properties of the tree efficiently, resulting in a much faster solution.
Here is the implementation of the `maxPathSum` method in various programming languages along with the analysis of time and space complexities.

### C++

```cpp
/**
 * Definition for a binary tree node.
 * struct TreeNode {
 *     int val;
 *     TreeNode *left;
 *     TreeNode *right;
 *     TreeNode() : val(0), left(nullptr), right(nullptr) {}
 *     TreeNode(int x) : val(x), left(nullptr, right(nullptr) {}
 *     TreeNode(int x, TreeNode *left, TreeNode *right) : val(x), left(left), right(right) {}
 };
 */

class Solution {
public:
    int maxPathSum(TreeNode* root) {
        int globalMax = INT_MIN;
        helper(root, globalMax);
        return globalMax;
    }
    
private:
    int helper(TreeNode* node, int& globalMax) {
        if (!node) return 0;
        
        int leftMax = max(helper(node->left, globalMax), 0);
        int rightMax = max(helper(node->right, globalMax), 0);
        
        int currentMax = node->val + leftMax + rightMax;
        globalMax = max(globalMax, currentMax);
        
        return node->val + max(leftMax, rightMax);
    }
};

// Time Complexity: O(n)
// Space Complexity: O(h) where h is the height of the tree.
```

### Java

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
    private int globalMax = Integer.MIN_VALUE;
    
    public int maxPathSum(TreeNode root) {
        helper(root);
        return globalMax;
    }
    
    private int helper(TreeNode node) {
        if (node == null) return 0;
        
        int leftMax = Math.max(helper(node.left), 0);
        int rightMax = Math.max(helper(node.right), 0);
        
        int currentMax = node.val + leftMax + rightMax;
        globalMax = Math.max(globalMax, currentMax);
        
        return node.val + Math.max(leftMax, rightMax);
    }
}

// Time Complexity: O(n)
// Space Complexity: O(h) where h is the height of the tree.
```

### Python

```python
# Definition for a binary tree node.
# class TreeNode(object):
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right

class Solution(object):
    def maxPathSum(self, root):
        """
        :type root: TreeNode
        :rtype: int
        """
        self.global_max = float('-inf')
        def helper(node):
            if node is None:
                return 0
            left_max = max(helper(node.left), 0)
            right_max = max(helper(node.right), 0)
            
            current_max = node.val + left_max + right_max
            self.global_max = max(self.global_max, current_max)
            
            return node.val + max(left_max, right_max)

        helper(root)
        return self.global_max

# Time Complexity: O(n)
# Space Complexity: O(h) where h is the height of the tree.
```

### Python 3

```python
# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right

class Solution:
    def maxPathSum(self, root: Optional[TreeNode]) -> int:
        self.global_max = float('-inf')

        def helper(node: Optional[TreeNode]) -> int:
            if node is None:
                return 0
            left_max = max(helper(node.left), 0)
            right_max = max(helper(node.right), 0)
            
            current_max = node.val + left_max + right_max
            self.global_max = max(self.global_max, current_max)
            
            return node.val + max(left_max, right_max)

        helper(root)
        return self.global_max

# Time Complexity: O(n)
# Space Complexity: O(h) where h is the height of the tree.
```

### C

```c
/**
 * Definition for a binary tree node.
 * struct TreeNode {
 *     int val;
 *     struct TreeNode *left;
 *     struct TreeNode *right;
 * };
 */

int helper(struct TreeNode* node, int* globalMax) {
    if (!node) return 0;

    int leftMax = fmax(helper(node->left, globalMax), 0);
    int rightMax = fmax(helper(node->right, globalMax), 0);
    
    int currentMax = node->val + leftMax + rightMax;
    *globalMax = fmax(*globalMax, currentMax);

    return node->val + fmax(leftMax, rightMax);
}

int maxPathSum(struct TreeNode* root) {
    int globalMax = INT_MIN;
    helper(root, &globalMax);
    return globalMax;
}

// Time Complexity: O(n)
// Space Complexity: O(h) where h is the height of the tree.
```

### C#

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
    private int globalMax = int.MinValue;
    
    public int MaxPathSum(TreeNode root) {
        Helper(root);
        return globalMax;
    }
    
    private int Helper(TreeNode node) {
        if (node == null) return 0;
        
        int leftMax = Math.Max(Helper(node.left), 0);
        int rightMax = Math.Max(Helper(node.right), 0);
        
        int currentMax = node.val + leftMax + rightMax;
        globalMax = Math.Max(globalMax, currentMax);
        
        return node.val + Math.Max(leftMax, rightMax);
    }
}

// Time Complexity: O(n)
// Space Complexity: O(h) where h is the height of the tree.
```

### JavaScript

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
 * @return {number}
 */
var maxPathSum = function(root) {
    let globalMax = -Infinity;

    const helper = node => {
        if (node === null) return 0;
        
        let leftMax = Math.max(helper(node.left), 0);
        let rightMax = Math.max(helper(node.right), 0);
        
        let currentMax = node.val + leftMax + rightMax;
        globalMax = Math.max(globalMax, currentMax);
        
        return node.val + Math.max(leftMax, rightMax);
    }
    
    helper(root);
    return globalMax;
};

// Time Complexity: O(n)
// Space Complexity: O(h) where h is the height of the tree.
```

### TypeScript

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

function maxPathSum(root: TreeNode | null): number {
    let globalMax = -Infinity;

    const helper = (node: TreeNode | null): number => {
        if (node === null) return 0;

        let leftMax = Math.max(helper(node.left), 0);
        let rightMax = Math.max(helper(node.right), 0);

        let currentMax = node.val + leftMax + rightMax;
        globalMax = Math.max(globalMax, currentMax);
        
        return node.val + Math.max(leftMax, rightMax);
    }

    helper(root);
    return globalMax;
}

// Time Complexity: O(n)
// Space Complexity: O(h) where h is the height of the tree.
```

### PHP

```php
<?php
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
    private $globalMax;

    /**
     * @param TreeNode $root
     * @return Integer
     */
    function maxPathSum($root) {
        $this->globalMax = PHP_INT_MIN;
        $this->helper($root);
        return $this->globalMax;
    }

    private function helper($node) {
        if ($node === null) return 0;

        $leftMax = max($this->helper($node->left), 0);
        $rightMax = max($this->helper($node->right), 0);

        $currentMax = $node->val + $leftMax + $rightMax;
        $this->globalMax = max($this->globalMax, $currentMax);

        return $node->val + max($leftMax, $rightMax);
    }
}

// Time Complexity: O(n)
// Space Complexity: O(h) where h is the height of the tree.
?>
```

### Swift

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
    func maxPathSum(_ root: TreeNode?) -> Int {
        var globalMax = Int.min
    
        @discardableResult
        func helper(_ node: TreeNode?) -> Int {
            guard let node = node else { return 0 }
        
            let leftMax = max(helper(node.left), 0)
            let rightMax = max(helper(node.right), 0)
        
            let currentMax = node.val + leftMax + rightMax
            globalMax = max(globalMax, currentMax)
        
            return node.val + max(leftMax, rightMax)
        }
    
        helper(root)
        return globalMax
    }
}

// Time Complexity: O(n)
// Space Complexity: O(h) where h is the height of the tree.
```

### Kotlin

```kotlin
/**
 * Example:
 * var ti = TreeNode(5)
 * var v = ti.`val`
 * Definition for a binary tree node.
 * class TreeNode(var `val`: Int) {
 *     var left: TreeNode? = null
 *     var right: TreeNode? = null
 * }
 */

class Solution {
    private var globalMax = Int.MIN_VALUE

    fun maxPathSum(root: TreeNode?): Int {
        helper(root)
        return globalMax
    }

    private fun helper(node: TreeNode?): Int {
        if (node == null) return 0
        
        val leftMax = max(helper(node.left), 0)
        val rightMax = max(helper(node.right), 0)
        
        val currentMax = node.`val` + leftMax + rightMax
        globalMax = max(globalMax, currentMax)
        
        return node.`val` + max(leftMax, rightMax)
    }
}

// Time Complexity: O(n)
// Space Complexity: O(h) where h is the height of the tree.
```

### Dart

```dart
class TreeNode {
  int val;
  TreeNode? left;
  TreeNode? right;
  TreeNode([this.val = 0, this.left, this.right]);
}

class Solution {
  int _globalMax = -9223372036854775807;
  
  int maxPathSum(TreeNode? root) {
    helper(root);
    return _globalMax;
  }

  int helper(TreeNode? node) {
    if (node == null) return 0;

    int leftMax = (helper(node.left)).clamp(0, double.infinity).toInt();
    int rightMax = (helper(node.right)).clamp(0, double.infinity).toInt();

    int currentMax = node.val + leftMax + rightMax;
    _globalMax = _globalMax.clamp(-9223372036854775807, currentMax).toInt();

    return node.val + leftMax.clamp(leftMax, rightMax);
  }
}

// Time Complexity: O(n)
// Space Complexity: O(h) where h is the height of the tree.
```

### Go

```go
type TreeNode struct {
    Val   int
    Left  *TreeNode
    Right *TreeNode
}

func maxPathSum(root *TreeNode) int {
    globalMax := ^int(^uint(0) >> 1)
    helper(root, &globalMax)
    return globalMax
}

func helper(node *TreeNode, globalMax *int) int {
    if node == nil {
        return 0
    }

    leftMax := max(helper(node.Left, globalMax), 0)
    rightMax := max(helper(node.Right, globalMax), 0)

    currentMax := node.Val + leftMax + rightMax
    *globalMax = max(*globalMax, currentMax)

    return node.Val + max(leftMax, rightMax)
}

func max(a, b int) int {
    if a > b {
        return a
    }
    return b
}

// Time Complexity: O(n)
// Space Complexity: O(h) where h is the height of the tree.
```

### Ruby

```ruby
# Definition for a binary tree node.
# class TreeNode
#   attr_accessor :val, :left, :right
#   def initialize(val = 0, left = nil, right = nil)
#     @val = val
#     @left = left
#     @right = right
#   end
# end

# @param {TreeNode} root
# @return {Integer}
def max_path_sum(root)
    @global_max = -Float::INFINITY
    helper(root)
    @global_max
end

def helper(node)
    return 0 unless node

    left_max = [helper(node.left), 0].max
    right_max = [helper(node.right), 0].max

    current_max = node.val + left_max + right_max
    @global_max = [@global_max, current_max].max

    node.val + [left_max, right_max].max
end

# Time Complexity: O(n)
# Space Complexity: O(h) where h is the height of the tree.
```

### Scala

```scala
import scala.math

class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
}

object Solution {
  def maxPathSum(root: TreeNode): Int = {
    var globalMax = Int.MinValue

    def helper(node: TreeNode): Int = {
      if (node == null) return 0

      val leftMax = math.max(helper(node.left), 0)
      val rightMax = math.max(helper(node.right), 0)

      val currentMax = node.value + leftMax + rightMax
      globalMax = math.max(globalMax, currentMax)

      node.value + math.max(leftMax, rightMax)
    }

    helper(root)
    globalMax
  }
}

// Time Complexity: O(n)
// Space Complexity: O(h) where h is the height of the tree.
```

### Rust

```rust
use std::rc::Rc;
use std::cell::RefCell;

// Definition for a binary tree node.
// #[derive(Debug, PartialEq, Eq)]
// pub struct TreeNode {
//   pub val: i32,
//   pub left: Option<Rc<RefCell<TreeNode>>>,
//   pub right: Option<Rc<RefCell<TreeNode>>>,
// }

// impl TreeNode {
//   #[inline]
//   pub fn new(val: i32) -> Self {
//     TreeNode {
//       val,
//       left: None,
//       right: None
//     }
//   }
// }

impl Solution {
    pub fn max_path_sum(root: Option<Rc<RefCell<TreeNode>>>) -> i32 {
        let mut global_max = i32::MIN;
        Solution::helper(root, &mut global_max);
        global_max
    }

    fn helper(node: Option<Rc<RefCell<TreeNode>>>, global_max: &mut i32) -> i32 {
        if let Some(n) = node {
            let n = n.borrow();
            let left_max = Solution::helper(n.left.clone(), global_max).max(0);
            let right_max = Solution::helper(n.right.clone(), global_max).max(0);

            let current_max = n.val + left_max + right_max;
            *global_max = (*global_max).max(current_max);

            return n.val + left_max.max(right_max);

        }
        0
    }
}

// Time Complexity: O(n)
// Space Complexity: O(h) where h is the height of the tree.
```

  
 
### Closing Statement

**Interviewer:** Excellent job! You've designed and implemented a well-optimized solution for finding the maximum path sum in a binary tree. Your initial analysis of the brute force approach provided a solid foundation, and your transition to an optimized solution utilizing depth-first search (DFS) effectively addressed the inefficiencies. You carefully considered both time and space complexities, ensuring your solution is both efficient and scalable.

### Summary of Key Points:
- The problem involves finding the maximum path sum in a binary tree, where paths can start and end at any node.
- A brute force approach would involve high time complexity (O(n^2)), making it impractical for large trees.
- The optimized approach uses DFS to keep track of maximum paths and ensures a time complexity of O(n) and a space complexity of O(h), where h is the tree height.

### Similar Questions:

1. **Path Sum:** Given a binary tree and a sum, determine if the tree has a root-to-leaf path such that adding up all the values along the path equals the given sum.
2. **Binary Tree Maximum Path Sum II:** Find the maximum path sum in a binary tree where the path must go through the root and can now traverse both left and right subtrees.
3. **Longest Univalue Path:** Find the length of the longest path where each node in the path has the same value.
4. **Diameter of Binary Tree:** Calculate the diameter of a binary tree, which is the length of the longest path between any two nodes in a tree (may or may not pass through the root).
5. **Binary Tree Paths:** Given a binary tree, return all root-to-leaf paths.

These related problems involve variations on tree traversal and path calculations, helping you further hone your skills in handling binary tree data structures. Keep up the excellent work, and continue practicing these advanced concepts to deepen your understanding.

