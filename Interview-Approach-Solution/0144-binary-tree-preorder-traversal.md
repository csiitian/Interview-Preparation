### Interviewer and Interviewee Discussion:

**Interviewer:** Today, we'll discuss a problem related to binary tree traversal. The problem is: Given the root of a binary tree, return the preorder traversal of its nodes' values.

**Interviewee:** Preorder traversal means visiting nodes in the order: root, left subtree, and then right subtree, correct?

**Interviewer:** That's right. Can you walk me through how you would approach solving this problem?

**Interviewee:** Sure. Initially, I would think about a brute force approach. A straightforward brute force way is to use a recursive function to traverse the tree in preorder. Here's what I'm thinking:

1. **Base Case:** If the node is `None`, return an empty list.
2. **Recursive Case:** Otherwise, visit the root node first, then recursively visit the left subtree, and finally the right subtree.

Would you like me to write the code for this approach?

**Interviewer:** Before diving into coding, let's talk about the time and space complexity of this recursive approach.

**Interviewee:** Sure. 

### Brute Force Approach: Recursive Preorder Traversal

For a recursive approach to perform a preorder traversal:
- **Time Complexity:** Each node is visited exactly once, thus the time complexity is \(O(n)\), where \(n\) is the number of nodes in the binary tree.

- **Space Complexity:** In the worst case, the space complexity is \(O(h)\), where \(h\) is the height of the tree. This space is used by the call stack during the recursion. For a balanced tree, \(h\) is \(O(\log n)\), and for a completely unbalanced tree, \(h\) could be \(O(n)\).

### Recursive Preorder Traversal Code:

```python
def preorderTraversal(root):
    if not root:
        return []
    return [root.val] + preorderTraversal(root.left) + preorderTraversal(root.right)
```

**Interviewer:** That's correct. Now, can you think of an iterative way to perform the preorder traversal?

**Interviewee:** Yes. The iterative approach can be achieved using a stack to mimic the call stack of the recursion. Here's the idea:

1. Initialize a stack and push the root node onto the stack.
2. Loop until the stack is empty:
   - Pop the node from the stack, add its value to the result list.
   - Push the right child of the node onto the stack.
   - Push the left child of the node onto the stack.

The order of pushing right first and then left ensures that the left subtree is processed first.

### Iterative Preorder Traversal Code:

```python
def preorderTraversal(root):
    if not root:
        return []
    
    stack = [root]
    result = []
    
    while stack:
        node = stack.pop()
        result.append(node.val)
        if node.right:
            stack.append(node.right)
        if node.left:
            stack.append(node.left)
    
    return result
```

**Interviewer:** Great, that’s exactly what I was looking for.

**Interviewee:** 

### Time and Space Complexity of Iterative Approach:
- **Time Complexity:** Each node is visited exactly once, so the time complexity remains \(O(n)\).
- **Space Complexity:** In the worst case, the space complexity is \(O(n)\), since at most all nodes could be in the stack.

### Visual Explanation:

Here’s a visual representation of the process:

**Example Tree:**

```
    1
     \
      2
     /
    3
```

**Traversal Process:**

1. Start with root node: `1`
    - Stack: [1]
    - Result: []
    
2. Pop `1`, add its value to result, push its right child `2`.
    - Stack: [2]
    - Result: [1]
    
3. Pop `2`, add its value to result, push its right child (none), and then push its left child `3`.
    - Stack: [3]
    - Result: [1, 2]
    
4. Pop `3`, add its value to result.
    - Stack: []
    - Result: [1, 2, 3]

Final output would be `[1, 2, 3]`.

This completes the traversal in preorder iteratively with the use of a stack.
Certainly! Below are the code implementations for each language provided, along with the time and space complexity for each.

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
#include <vector>
#include <stack>

class Solution {
public:
    std::vector<int> preorderTraversal(TreeNode* root) {
        if (!root) return {};

        std::vector<int> result;
        std::stack<TreeNode*> stack;
        stack.push(root);
        
        while (!stack.empty()) {
            TreeNode* node = stack.top();
            stack.pop();
            result.push_back(node->val);
            if (node->right) stack.push(node->right);
            if (node->left) stack.push(node->left);
        }
        
        return result;
    }
};
// Time Complexity: O(n)
// Space Complexity: O(n)
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
import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

class Solution {
    public List<Integer> preorderTraversal(TreeNode root) {
        List<Integer> result = new ArrayList<>();
        if (root == null) return result;
        
        Stack<TreeNode> stack = new Stack<>();
        stack.push(root);
        
        while (!stack.isEmpty()) {
            TreeNode node = stack.pop();
            result.add(node.val);
            if (node.right != null) stack.push(node.right);
            if (node.left != null) stack.push(node.left);
        }
        
        return result;
    }
}
// Time Complexity: O(n)
// Space Complexity: O(n)
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
    def preorderTraversal(self, root):
        """
        :type root: TreeNode
        :rtype: List[int]
        """
        if not root: return []
        
        stack, result = [root], []
        
        while stack:
            node = stack.pop()
            result.append(node.val)
            if node.right: stack.append(node.right)
            if node.left: stack.append(node.left)
        
        return result
# Time Complexity: O(n)
# Space Complexity: O(n)
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
    def preorderTraversal(self, root: Optional[TreeNode]) -> List[int]:
        if not root: return []
        
        stack, result = [root], []
        
        while stack:
            node = stack.pop()
            result.append(node.val)
            if node.right: stack.append(node.right)
            if node.left: stack.append(node.left)
        
        return result
# Time Complexity: O(n)
# Space Complexity: O(n)
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
/**
 * Note: The returned array must be malloced, assume caller calls free().
 */
#include <stdlib.h>

int* preorderTraversal(struct TreeNode* root, int* returnSize) {
    int* result = (int*)malloc(100 * sizeof(int));
    struct TreeNode** stack = (struct TreeNode**)malloc(100 * sizeof(struct TreeNode*));
    int stackSize = 0;
    *returnSize = 0;

    if (root) stack[stackSize++] = root;

    while (stackSize > 0) {
        struct TreeNode* node = stack[--stackSize];
        result[(*returnSize)++] = node->val;
        if (node->right) stack[stackSize++] = node->right;
        if (node->left) stack[stackSize++] = node->left;
    }

    free(stack);
    return result;
}
// Time Complexity: O(n)
// Space Complexity: O(n)
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
using System.Collections.Generic;

public class Solution {
    public IList<int> PreorderTraversal(TreeNode root) {
        List<int> result = new List<int>();
        if (root == null) return result;
        
        Stack<TreeNode> stack = new Stack<TreeNode>();
        stack.Push(root);
        
        while (stack.Count > 0) {
            TreeNode node = stack.Pop();
            result.Add(node.val);
            if (node.right != null) stack.Push(node.right);
            if (node.left != null) stack.Push(node.left);
        }
        
        return result;
    }
}
// Time Complexity: O(n)
// Space Complexity: O(n)
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
 * @return {number[]}
 */
var preorderTraversal = function(root) {
    if (root == null) return [];

    let stack = [root], result = [];

    while (stack.length > 0) {
        let node = stack.pop();
        result.push(node.val);
        if (node.right != null) stack.push(node.right);
        if (node.left != null) stack.push(node.left);
    }

    return result;
};
// Time Complexity: O(n)
// Space Complexity: O(n)
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

function preorderTraversal(root: TreeNode | null): number[] {
    if (root === null) return [];

    let result: number[] = [];
    let stack: TreeNode[] = [root];

    while (stack.length) {
        let node = stack.pop()!;
        result.push(node.val);
        if (node.right !== null) stack.push(node.right);
        if (node.left !== null) stack.push(node.left);
    }

    return result;
}
// Time Complexity: O(n)
// Space Complexity: O(n)
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
     * @return Integer[]
     */
    function preorderTraversal($root) {
        $result = [];
        if ($root === null) return $result;

        $stack = [$root];

        while (!empty($stack)) {
            $node = array_pop($stack);
            $result[] = $node->val;
            if ($node->right !== null) $stack[] = $node->right;
            if ($node->left !== null) $stack[] = $node->left;
        }

        return $result;
    }
}
// Time Complexity: O(n)
// Space Complexity: O(n)
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
    func preorderTraversal(_ root: TreeNode?) -> [Int] {
        var result: [Int] = []
        guard let root = root else { return result }

        var stack: [TreeNode] = [root]

        while !stack.isEmpty {
            let node = stack.removeLast()
            result.append(node.val)
            if let right = node.right { stack.append(right) }
            if let left = node.left { stack.append(left) }
        }

        return result
    }
}
// Time Complexity: O(n)
// Space Complexity: O(n)
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
    fun preorderTraversal(root: TreeNode?): List<Int> {
        val result = mutableListOf<Int>()
        if (root == null) return result

        val stack = ArrayDeque<TreeNode>()
        stack.add(root)

        while (stack.isNotEmpty()) {
            val node = stack.removeLast()
            result.add(node.`val`)
            node.right?.let { stack.add(it) }
            node.left?.let { stack.add(it) }
        }

        return result
    }
}
// Time Complexity: O(n)
// Space Complexity: O(n)
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
  List<int> preorderTraversal(TreeNode? root) {
    List<int> result = [];
    if (root == null) return result;

    List<TreeNode> stack = [root];

    while (stack.isNotEmpty) {
        TreeNode node = stack.removeLast();
        result.add(node.val);
        if (node.right != null) stack.add(node.right!);
        if (node.left != null) stack.add(node.left!);
    }

    return result;
  }
}
// Time Complexity: O(n)
// Space Complexity: O(n)
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
func preorderTraversal(root *TreeNode) []int {
    if root == nil { return []int{} }

    var result []int
    stack := []*TreeNode{root}

    for len(stack) > 0 {
        node := stack[len(stack)-1]
        stack = stack[:len(stack)-1]
        result = append(result, node.Val)
        if node.Right != nil { stack = append(stack, node.Right) }
        if node.Left != nil { stack = append(stack, node.Left) }
    }

    return result
}
// Time Complexity: O(n)
// Space Complexity: O(n)
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
# @return {Integer[]}
def preorder_traversal(root)
    return [] if root.nil?

    result = []
    stack = [root]

    until stack.empty?
        node = stack.pop
        result << node.val
        stack << node.right if node.right
        stack << node.left if node.left
    end

    result
end
# Time Complexity: O(n)
# Space Complexity: O(n)
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
    def preorderTraversal(root: TreeNode): List[Int] = {
        if (root == null) return List.empty[Int]

        var result = List.empty[Int]
        var stack = List(root)

        while (stack.nonEmpty) {
            val node = stack.head
            stack = stack.tail
            result = result :+ node.value
            if (node.right != null) stack = node.right :: stack
            if (node.left != null) stack = node.left :: stack
        }

        result
    }
}
// Time Complexity: O(n)
// Space Complexity: O(n)
```

### Rust

```rust
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
    pub fn preorder_traversal(root: Option<Rc<RefCell<TreeNode>>>) -> Vec<i32> {
        let mut result = vec![];
        let mut stack = vec![];

        if let Some(node) = root {
            stack.push(node);
        }

        while let Some(node) = stack.pop() {
            let node = node.borrow();
            result.push(node.val);
            if let Some(right) = node.right.clone() {
                stack.push(right);
            }
            if let Some(left) = node.left.clone() {
                stack.push(left);
            }
        }

        result
    }
}
// Time Complexity: O(n)
// Space Complexity: O(n)
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

(define/contract (preorder-traversal root)
  (-> (or/c tree-node? #f) (listof exact-integer?))

  (define result '())

  (define (helper node)
    (when node
      (set! result (append result (list (tree-node-val node))))
      (helper (tree-node-left node))
      (helper (tree-node-right node))))

  (helper root)
  result)
; Time Complexity: O(n)
; Space Complexity: O(n)
```

### Erlang

```erlang
%% Definition for a binary tree node.
%%
%% -record(tree_node, {val = 0 :: integer(),
%%                     left = null  :: 'null' | #tree_node{},
%%                     right = null :: 'null' | #tree_node{}}).

-spec preorder_traversal(Root :: #tree_node{} | null) -> [integer()].
preorder_traversal(Root) ->
    preorder_traversal_helper(Root, []).

preorder_traversal_helper(null, Acc) ->
    lists:reverse(Acc);
preorder_traversal_helper(#tree_node{val = Val, left = Left, right = Right}, Acc) ->
    Acc1 = [Val | Acc],
    Acc2 = preorder_traversal_helper(Right, Acc1),
    preorder_traversal_helper(Left, Acc2).
% Time Complexity: O(n)
% Space Complexity: O(n)
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
  @spec preorder_traversal(root :: TreeNode.t | nil) :: [integer]
  def preorder_traversal(root) do
    preorder_traversal_helper(root, [])
    |> Enum.reverse()
  end

  defp preorder_traversal_helper(nil, acc), do: acc
  defp preorder_traversal_helper(%TreeNode{val: v, left: l, right: r}, acc) do
    preorder_traversal_helper(l, [v | preorder_traversal_helper(r, acc)])
  end
end
# Time Complexity: O(n)
# Space Complexity: O(n)
```

Each solution maintains the intended \(O(n)\) time complexity and \(O(n)\) space complexity due to the stack usage for iterative approaches or the call stack for recursive approaches.


### Closing Statement:

**Interviewer:** Great job today! You've successfully implemented and discussed various approaches for performing a preorder traversal of a binary tree. From the initial brute force method to the optimized iterative method, you demonstrated a solid understanding of both the recursive and iterative approaches, as well as their time and space complexities. You've also showcased your ability to convert these solutions into multiple programming languages efficiently. This ability is valuable, as it shows your versatility in software development. Keep practicing, and you'll continue to improve your problem-solving skills.

**Interviewee:** Thank you! It was an excellent exercise, and I appreciate the opportunity to work through these problems and explore their complexities. I'll make sure to review similar problems to further enhance my understanding of tree traversals and related data structures.

### Similar Questions:

1. **Inorder Traversal of a Binary Tree**:
   - Given the root of a binary tree, return the inorder traversal of its nodes' values.

2. **Postorder Traversal of a Binary Tree**:
   - Given the root of a binary tree, return the postorder traversal of its nodes' values.

3. **Level Order Traversal of a Binary Tree (Breadth-First Search)**:
   - Given the root of a binary tree, return its level order traversal. (i.e., from left to right, level by level).

4. **Iterative Inorder Traversal of a Binary Tree**:
   - Implement the inorder traversal of a binary tree iteratively without using recursion.

5. **Binary Tree Paths**:
   - Given a binary tree, return all root-to-leaf paths.

6. **Maximum Depth of Binary Tree**:
   - Given a binary tree, return its maximum depth.

7. **Sum of Left Leaves**:
   - Find the sum of all left leaves in a given binary tree.

8. **Path Sum**:
   - Given the root of the binary tree and an integer targetSum, return true if the tree has a root-to-leaf path such that adding up all the values along the path equals targetSum.

Working through these problems will help solidify your understanding of tree data structures and various traversal techniques, ensuring you're well-prepared for similar tasks in the future.