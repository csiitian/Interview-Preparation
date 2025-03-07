### Interviewer and Interviewee Discussion

**Interviewer:** Let's discuss the problem of finding the maximum depth of a binary tree given its root.

**Interviewee:** Sure. The maximum depth of a binary tree is defined as the number of nodes along the longest path from the root node down to the farthest leaf node. 

**Interviewer:** Correct. Can you give me an example and explain the expected output?

**Interviewee:** Certainly. For example, let's consider a binary tree where the root node is 3. It has two children, 9 and 20, and the 20 node further has two children, 15 and 7. The tree looks like this:

```
      3
     / \
    9  20
       / \
      15  7
```
The maximum depth here is 3, as the longest path from 3 to the farthest leaf node is 3 -> 20 -> 15 (or 3 -> 20 -> 7).

**Interviewer:** Great. How would you initially approach solving this problem using a brute force method?

**Interviewee:** The brute force approach would be to traverse the tree and calculate the depth of each path. We can use Depth-First Search (DFS) for this. We would recursively call DFS on each node and keep track of the depth level. The base case would be when we reach a leaf node (i.e., a node with no children).

**Interviewer:** That makes sense. Can you describe the time and space complexity of this brute force method?

**Interviewee:** Sure. For the DFS approach:

- **Time Complexity:** O(N), where N is the number of nodes in the tree because we need to visit each node once.
- **Space Complexity:** O(H), where H is the height of the tree. In the worst case, this could be O(N) for a skewed tree, but for a balanced tree, it’s O(log N).

### More Efficient Approach

**Interviewer:** Can we optimize this approach further? 

**Interviewee:** The DFS approach is already quite efficient. However, we can consider an iterative approach using Breadth-First Search (BFS). Using BFS, we explore each level of the tree one by one.

### Recursive Approach (DFS)
Let's first look at the recursive approach using DFS.

```python
class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

def maxDepth(root: TreeNode) -> int:
    if not root:
        return 0
    left_depth = maxDepth(root.left)
    right_depth = maxDepth(root.right)
    return max(left_depth, right_depth) + 1
```

This approach is simple and leverages recursion to explore each path from the root to the leaf nodes.

### Iterative Approach (BFS)
Now, let's consider the BFS approach.

```python
from collections import deque

def maxDepth(root: TreeNode) -> int:
    if not root:
        return 0
    
    queue = deque([root])
    depth = 0
    
    while queue:
        depth += 1
        level_length = len(queue)
        for _ in range(level_length):
            node = queue.popleft()
            if node.left:
                queue.append(node.left)
            if node.right:
                queue.append(node.right)
    
    return depth
```

In this approach:

- **Time Complexity:** O(N), as each node is processed once.
- **Space Complexity:** O(N), representing the maximum width of the tree (in the worst case, all leaf nodes at a level).

### Visualization

Let's visualize the BFS approach on the example tree:

```
      3
     / \
    9  20
       / \
      15  7
```

1. Initialize Queue: `queue = [3]`, `depth = 0`
2. Process Level 1: `depth = 1`, `queue = [9, 20]`
3. Process Level 2: `depth = 2`, `queue = [15, 7]`
4. Process Level 3: `depth = 3`, `queue = []`

The final depth is 3, which is the maximum depth of the tree.

By discussing both the DFS and BFS approaches, we've covered efficient solutions for finding the maximum depth of a binary tree.
Sure, let's provide the implementations in various languages. We'll use the recursive Depth First Search (DFS) approach to calculate the maximum depth.

### C++
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
    int maxDepth(TreeNode* root) {
        if (!root) return 0;
        int leftDepth = maxDepth(root->left);
        int rightDepth = maxDepth(root->right);
        return max(leftDepth, rightDepth) + 1;
    }
};
// Time Complexity: O(N)
// Space Complexity: O(H), where H is the height of the tree.
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
    public int maxDepth(TreeNode root) {
        if (root == null) return 0;
        int leftDepth = maxDepth(root.left);
        int rightDepth = maxDepth(root.right);
        return Math.max(leftDepth, rightDepth) + 1;
    }
}
// Time Complexity: O(N)
// Space Complexity: O(H), where H is the height of the tree.
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
    def maxDepth(self, root):
        """
        :type root: TreeNode
        :rtype: int
        """
        if not root:
            return 0
        left_depth = self.maxDepth(root.left)
        right_depth = self.maxDepth(root.right)
        return max(left_depth, right_depth) + 1
# Time Complexity: O(N)
# Space Complexity: O(H), where H is the height of the tree.
```

### Python3
```python
# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right
class Solution:
    def maxDepth(self, root: Optional[TreeNode]) -> int:
        if not root:
            return 0
        left_depth = self.maxDepth(root.left)
        right_depth = self.maxDepth(root.right)
        return max(left_depth, right_depth) + 1
# Time Complexity: O(N)
# Space Complexity: O(H), where H is the height of the tree.
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
int maxDepth(struct TreeNode* root) {
    if (root == NULL) return 0;
    int leftDepth = maxDepth(root->left);
    int rightDepth = maxDepth(root->right);
    return (leftDepth > rightDepth ? leftDepth : rightDepth) + 1;
}
// Time Complexity: O(N)
// Space Complexity: O(H), where H is the height of the tree.
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
    public int MaxDepth(TreeNode root) {
        if (root == null) return 0;
        int leftDepth = MaxDepth(root.left);
        int rightDepth = MaxDepth(root.right);
        return Math.Max(leftDepth, rightDepth) + 1;
    }
}
// Time Complexity: O(N)
// Space Complexity: O(H), where H is the height of the tree.
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
var maxDepth = function(root) {
    if (root === null) return 0;
    let leftDepth = maxDepth(root.left);
    let rightDepth = maxDepth(root.right);
    return Math.max(leftDepth, rightDepth) + 1;
};
// Time Complexity: O(N)
// Space Complexity: O(H), where H is the height of the tree.
```

### TypeScript
```typescript
/**
 * Definition for a binary tree node.
 * class TreeNode {
 *     val: number;
 *     left: TreeNode | null;
 *     right: TreeNode | null;
 *     constructor(val?: number, left?: TreeNode | null, right?: TreeNode | null) {
 *         this.val = (val===undefined ? 0 : val);
 *         this.left = (left===undefined ? null : left);
 *         this.right = (right===undefined ? null : right);
 *     }
 * }
 */

function maxDepth(root: TreeNode | null): number {
    if (root === null) return 0;
    let leftDepth = maxDepth(root.left);
    let rightDepth = maxDepth(root.right);
    return Math.max(leftDepth, rightDepth) + 1;
}
// Time Complexity: O(N)
// Space Complexity: O(H), where H is the height of the tree.
```

### PHP
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
     * @return Integer
     */
    function maxDepth($root) {
        if ($root == null) return 0;
        $leftDepth = $this->maxDepth($root->left);
        $rightDepth = $this->maxDepth($root->right);
        return max($leftDepth, $rightDepth) + 1;
    }
}
// Time Complexity: O(N)
// Space Complexity: O(H), where H is the height of the tree.
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
    func maxDepth(_ root: TreeNode?) -> Int {
        if root == nil { return 0 }
        let leftDepth = maxDepth(root?.left)
        let rightDepth = maxDepth(root?.right)
        return max(leftDepth, rightDepth) + 1
    }
}
// Time Complexity: O(N)
// Space Complexity: O(H), where H is the height of the tree.
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
    fun maxDepth(root: TreeNode?): Int {
        if (root == null) return 0
        val leftDepth = maxDepth(root.left)
        val rightDepth = maxDepth(root.right)
        return maxOf(leftDepth, rightDepth) + 1
    }
}
// Time Complexity: O(N)
// Space Complexity: O(H), where H is the height of the tree.
```

### Dart
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
  int maxDepth(TreeNode? root) {
    if (root == null) return 0;
    int leftDepth = maxDepth(root.left);
    int rightDepth = maxDepth(root.right);
    return Math.max(leftDepth, rightDepth) + 1;
  }
}
// Time Complexity: O(N)
// Space Complexity: O(H), where H is the height of the tree.
```

### Go
```go
/**
 * Definition for a binary tree node.
 * type TreeNode struct {
 *     Val int
 *     Left *TreeNode
 *     Right *TreeNode
 * }
 */
func maxDepth(root *TreeNode) int {
    if root == nil {
        return 0
    }
    leftDepth := maxDepth(root.Left)
    rightDepth := maxDepth(root.Right)
    if leftDepth > rightDepth {
        return leftDepth + 1
    } else {
        return rightDepth + 1
    }
}
// Time Complexity: O(N)
// Space Complexity: O(H), where H is the height of the tree.
```

### Ruby
```ruby
# Definition for a binary tree node.
# class TreeNode
#     attr_accessor :val, :left, :right
#     def initialize(val = 0, left = nil, right = nil)
#         @val = val
#         @left = left
#         @right = right
#     end
# end
# @param {TreeNode} root
# @return {Integer}
def max_depth(root)
    return 0 if root.nil?
    left_depth = max_depth(root.left)
    right_depth = max_depth(root.right)
    return [left_depth, right_depth].max + 1
end
# Time Complexity: O(N)
# Space Complexity: O(H), where H is the height of the tree.
```

### Scala
```scala
/**
 * Definition for a binary tree node.
 * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
 *   var value: Int = _value
 *   var left: TreeNode = _left
 *   var right: TreeNode = _right
 * }
 */
object Solution {
    def maxDepth(root: TreeNode): Int = {
        if (root == null) return 0
        val leftDepth = maxDepth(root.left)
        val rightDepth = maxDepth(root.right)
        return Math.max(leftDepth, rightDepth) + 1
    }
}
// Time Complexity: O(N)
// Space Complexity: O(H), where H is the height of the tree.
```

### Rust
```rust
// Definition for a binary tree node.
// #[derive(Debug, PartialEq, Eq)]
// pub struct TreeNode {
//   pub val: i32,
//   pub left: Option<Rc<RefCell<TreeNode>>>,
//   pub right: Option<Rc<RefCell<TreeNode>>>,
// }
// 
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
use std::rc::Rc;
use std::cell::RefCell;

impl Solution {
    pub fn max_depth(root: Option<Rc<RefCell<TreeNode>>>) -> i32 {
        match root {
            None => 0,
            Some(node) => {
                let left_depth = Solution::max_depth(node.borrow().left.clone());
                let right_depth = Solution::max_depth(node.borrow().right.clone());
                std::cmp::max(left_depth, right_depth) + 1
            }
        }
    }
}
// Time Complexity: O(N)
// Space Complexity: O(H), where H is the height of the tree.
```

### Racket
```racket
; Definition for a binary tree node.
#|
; val : integer?
; left : (or/c tree-node? #f)
; right : (or/c tree-node? #f)
(struct tree-node
  (val left right) #:mutable #:transparent)

; constructor
(define (make-tree-node [val 0])
  (tree-node val #f #f))
|#

(define/contract (max-depth root)
  (-> (or/c tree-node? #f) exact-integer?)
  (if (not root) 0
      (+ 1 (max (max-depth (tree-node-left root))
                (max-depth (tree-node-right root))))))
; Time Complexity: O(N)
; Space Complexity: O(H), where H is the height of the tree.
```

### Erlang
```erlang
%% Definition for a binary tree node.
%%
%% -record(tree_node, {val = 0 :: integer(),
%%                     left = null  :: 'null' | #tree_node{},
%%                     right = null :: 'null' | #tree_node{}}).

-spec max_depth(Root :: #tree_node{} | null) -> integer().
max_depth(null) -> 
    0;
max_depth(#tree_node{left = Left, right = Right}) ->
    1 + max(max_depth(Left), max_depth(Right)).
% Time Complexity: O(N)
% Space Complexity: O(H), where H is the height of the tree.
```

### Elixir
```elixir
# Definition for a binary tree node.
#
# defmodule TreeNode do
#   @type t :: %__MODULE__{
#           val: integer,
#           left: TreeNode.t() | nil,
#           right: TreeNode.t() | nil
#         }
#   defstruct val: 0, left: nil, right: nil
# end

defmodule Solution do
  @spec max_depth(root :: TreeNode.t() | nil) :: integer()
  def max_depth(nil), do: 0
  def max_depth(%TreeNode{left: left, right: right}) do
    1 + max(max_depth(left), max_depth(right))
  end
end
# Time Complexity: O(N)
# Space Complexity: O(H), where H is the height of the tree.
```

Each implementation follows the same logic: recursively find the maximum depth of the left and right subtrees, then return the maximum of these depths plus one. This ensures that we correctly find the maximum depth of the binary tree.


### Closing Statement

Congratulations! We've successfully implemented the solution to find the maximum depth of a binary tree in multiple programming languages. By leveraging Depth First Search (DFS), you'll efficiently explore all nodes and determine the longest path from the root to the farthest leaf node. The recursive approach is conducive for this problem due to its simplicity and readability, while ensuring optimal time and space complexity. These implementations serve as a robust foundation for understanding tree traversal techniques and can be extended to more complex operations on binary trees.

### Similar Questions

1. **Minimum Depth of Binary Tree**:
   - Given the root of a binary tree, determine its minimum depth - the number of nodes along the shortest path from the root to the nearest leaf node.

2. **Path Sum**:
   - Given the root of a binary tree and an integer `sum`, determine if the tree has a root-to-leaf path such that adding up all the values along the path equals the given sum.

3. **Binary Tree Level Order Traversal**:
   - Given the root of a binary tree, return the level order traversal of its nodes' values. (i.e., from left to right, level by level).

4. **Symmetric Tree**:
   - Check if a given binary tree is a mirror of itself (i.e., symmetric around its center).

5. **Binary Tree Inorder Traversal**:
   - Given the root of a binary tree, return its inorder traversal.

6. **Binary Tree Maximum Path Sum**:
   - Given a non-empty binary tree, find the maximum path sum. The path may start and end at any node in the tree.

Each of these problems helps deepen your understanding of binary tree structures and traversals, building a strong foundation for more advanced tree-based algorithms. Happy coding!