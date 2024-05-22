### Interviewer and Interviewee Discussion

**Interviewer:** Let's discuss the problem of performing a postorder traversal on a binary tree. Given the root of a binary tree, you need to return its nodes' values in postorder traversal. Do you understand the problem statement?

**Interviewee:** Yes, I believe so. In postorder traversal, we visit the nodes in the order: left subtree, right subtree, and then the root node. So, for the binary tree provided, for example, [1, null, 2, 3], I need to return [3, 2, 1].

**Interviewer:** Correct. Could you first explain how you would approach this problem using a brute force method?

### Initial Thoughts about Brute Force Approach

**Interviewee:** Sure. For the brute force method, we can use a simple recursive function. The postorder traversal can be represented using recursion where:
1. We traverse the left subtree.
2. Then we traverse the right subtree.
3. Finally, we add the root node value to our result.

Here's the recursive pseudocode for brute force:

```python
def postorder_traversal(root):
    result = []
    def traverse(node):
        if node:
            traverse(node.left)  # Traverse left subtree
            traverse(node.right) # Traverse right subtree
            result.append(node.val) # Visit root node
    traverse(root)
    return result
```

**Interviewer:** That appears to be a strategy. What do you think about the time and space complexity of this approach?

### Complexity Analysis

**Interviewee:**
- **Time Complexity:** Each node is visited exactly once, so the time complexity is O(n), where n is the number of nodes in the binary tree.
- **Space Complexity:** The space complexity depends on the depth of the recursion, which could be O(n) in the worst case (for a completely unbalanced tree, i.e., a linked list) and O(log n) for a balanced tree due to the recursion stack.

### Optimizing with Iterative Approach

**Interviewer:** Great. The recursive solution is pretty straightforward. As a follow-up, could you solve this problem iteratively?

**Interviewee:** Absolutely. We can use a stack to simulate the postorder traversal iteratively. Here's the approach:
1. We use two stacks: one for processing nodes (`stack1`) and one for gathering the results in reverse postorder (`stack2`).
2. We push the nodes onto `stack1` and then move through the nodes while pushing the left and right children onto `stack1` as we go.
3. Once a node is processed, it is added to `stack2`.
4. Finally, we pop all the elements from `stack2` to get the result in postorder.

Here's the pseudocode for the iterative approach:

```python
def postorder_traversal(root):
    if not root:
        return []
    
    stack1 = [root]
    stack2 = []
    while stack1:
        node = stack1.pop()
        stack2.append(node.val)
        if node.left:
            stack1.append(node.left)
        if node.right:
            stack1.append(node.right)
    
    result = []
    while stack2:
        result.append(stack2.pop())
    return result
```

### Illustration

```
Given Tree:
    1
     \
      2
     /
    3

1. Push root to stack1:
   stack1: [1]
   stack2: []

2. Process root (1), push it to stack2, push right (2) to stack1:
   stack1: [2]
   stack2: [1]
   
3. Process 2, push it to stack2, push left (3) to stack1:
   stack1: [3]
   stack2: [2, 1]

4. Process 3, push it to stack2:
   stack1: []
   stack2: [3, 2, 1]

5. Pop from stack2 for result:
   result: [3, 2, 1]
```

**Interviewer:** Excellent explanation and walkthrough. You've covered the brute force approach with recursion and its complexities, and then optimized it with an iterative method using stacks. Nicely done!
Sure, I'll provide the code snippets for postorder traversal in all the requested languages:

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
 * };
 */
class Solution {
public:
    vector<int> postorderTraversal(TreeNode* root) {
        vector<int> result;
        if (!root) return result;
        
        stack<TreeNode*> stack1;
        stack<TreeNode*> stack2;
        stack1.push(root);
        
        while (!stack1.empty()) {
            TreeNode* node = stack1.top();
            stack1.pop();
            stack2.push(node);
            
            if (node->left) stack1.push(node->left);
            if (node->right) stack1.push(node->right);
        }
        
        while (!stack2.empty()) {
            result.push_back(stack2.top()->val);
            stack2.pop();
        }
        
        return result;
    }
};
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
    public List<Integer> postorderTraversal(TreeNode root) {
        List<Integer> result = new ArrayList<>();
        if (root == null) return result;
        
        Stack<TreeNode> stack1 = new Stack<>();
        Stack<TreeNode> stack2 = new Stack<>();
        stack1.push(root);
        
        while (!stack1.isEmpty()) {
            TreeNode node = stack1.pop();
            stack2.push(node);
            
            if (node.left != null) stack1.push(node.left);
            if (node.right != null) stack1.push(node.right);
        }
        
        while (!stack2.isEmpty()) {
            result.add(stack2.pop().val);
        }
        
        return result;
    }
}
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
    def postorderTraversal(self, root: Optional[TreeNode]) -> List[int]:
        if not root:
            return []
        
        stack1 = [root]
        stack2 = []
        while stack1:
            node = stack1.pop()
            stack2.append(node.val)
            if node.left:
                stack1.append(node.left)
            if node.right:
                stack1.append(node.right)
        
        return stack2[::-1]
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
int* postorderTraversal(struct TreeNode* root, int* returnSize) {
	if (root == NULL) {
        *returnSize = 0;
        return NULL;
    }

    struct TreeNode** stack1 = (struct TreeNode**)malloc(100 * sizeof(struct TreeNode*));
    struct TreeNode** stack2 = (struct TreeNode**)malloc(100 * sizeof(struct TreeNode*));
    int index1 = 0, index2 = 0;
    int* result = (int*)malloc(100 * sizeof(int));
    
    stack1[index1++] = root;
    
    while (index1 > 0) {
        struct TreeNode* node = stack1[--index1];
        stack2[index2++] = node;
        
        if (node->left) stack1[index1++] = node->left;
        if (node->right) stack1[index1++] = node->right;
    }
    
    *returnSize = index2;
    for (int i = 0; i < index2; ++i) {
        result[i] = stack2[--index2]->val;
    }
    
    free(stack1);
    free(stack2);
    return result;
}
```

### C#
```csharp
using System.Collections.Generic;
using System.Linq;

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
    public IList<int> PostorderTraversal(TreeNode root) {
        List<int> result = new List<int>();
        if (root == null) return result;
        
        Stack<TreeNode> stack1 = new Stack<TreeNode>();
        Stack<TreeNode> stack2 = new Stack<TreeNode>();
        stack1.Push(root);
        
        while (stack1.Count > 0) {
            TreeNode node = stack1.Pop();
            stack2.Push(node);
            
            if (node.left != null) stack1.Push(node.left);
            if (node.right != null) stack1.Push(node.right);
        }
        
        while (stack2.Count > 0) {
            result.Add(stack2.Pop().val);
        }
        
        return result;
    }
}
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
var postorderTraversal = function(root) {
    if (!root) return [];
    
    let stack1 = [root];
    let stack2 = [];
    let result = [];
    
    while (stack1.length > 0) {
        let node = stack1.pop();
        stack2.push(node);
        if (node.left) stack1.push(node.left);
        if (node.right) stack1.push(node.right);
    }
    
    while (stack2.length > 0) {
        result.push(stack2.pop().val);
    }
    
    return result;
};
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

function postorderTraversal(root: TreeNode | null): number[] {
    if (!root) return [];
    
    let stack1: TreeNode[] = [root];
    let stack2: TreeNode[] = [];
    let result: number[] = [];
    
    while (stack1.length > 0) {
        let node = stack1.pop()!;
        stack2.push(node);
        if (node.left) stack1.push(node.left);
        if (node.right) stack1.push(node.right);
    }
    
    while (stack2.length > 0) {
        result.push(stack2.pop()!.val);
    }
    
    return result;
};
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
    function postorderTraversal($root) {
        $result = [];
        if (!$root) return $result;
        
        $stack1 = new SplStack();
        $stack2 = new SplStack();
        $stack1->push($root);
        
        while (!$stack1->isEmpty()) {
            $node = $stack1->pop();
            $stack2->push($node);
            
            if ($node->left !== null) $stack1->push($node->left);
            if ($node->right !== null) $stack1->push($node->right);
        }
        
        while (!$stack2->isEmpty()) {
            $result[] = $stack2->pop()->val;
        }
        
        return $result;
    }
}
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
    func postorderTraversal(_ root: TreeNode?) -> [Int] {
        var result = [Int]()
        guard let root = root else { return result }
        
        var stack1 = [root]
        var stack2 = [TreeNode]()
        
        while !stack1.isEmpty {
            let node = stack1.removeLast()
            stack2.append(node)
            
            if let left = node.left {
                stack1.append(left)
            }
            if let right = node.right {
                stack1.append(right)
            }
        }
        
        while !stack2.isEmpty {
            result.append(stack2.removeLast().val)
        }
        
        return result
    }
}
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
    fun postorderTraversal(root: TreeNode?): List<Int> {
        val result = mutableListOf<Int>()
        if (root == null) return result
        
        val stack1 = Stack<TreeNode>()
        val stack2 = Stack<TreeNode>()
        
        stack1.push(root)
        
        while (stack1.isNotEmpty()) {
            val node = stack1.pop()
            stack2.push(node)
            node.left?.let { stack1.push(it) }
            node.right?.let { stack1.push(it) }
        }
        
        while (stack2.isNotEmpty()) {
            result.add(stack2.pop().`val`)
        }
        
        return result
    }
}
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
  List<int> postorderTraversal(TreeNode? root) {
    List<int> result = [];
    if (root == null) return result;
    
    List<TreeNode?> stack1 = [root];
    List<TreeNode?> stack2 = [];
    
    while (stack1.isNotEmpty) {
      TreeNode? node = stack1.removeLast();
      stack2.add(node);
      if (node!.left != null) stack1.add(node.left);
      if (node.right != null) stack1.add(node.right);
    }
    
    while (stack2.isNotEmpty) {
      result.add(stack2.removeLast()!.val);
    }
    
    return result;
  }
}
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
func postorderTraversal(root *TreeNode) []int {
    if root == nil {
        return []int{}
    }
    
    stack1 := []*TreeNode{root}
    stack2 := []*TreeNode{}
    result := []int{}
    
    for len(stack1) > 0 {
        node := stack1[len(stack1)-1]
        stack1 = stack1[:len(stack1)-1]
        stack2 = append(stack2, node)
        
        if node.Left != nil {
            stack1 = append(stack1, node.Left)
        }
        if node.Right != nil {
            stack1 = append(stack1, node.Right)
        }
    }
    
    for len(stack2) > 0 {
        result = append(result, stack2[len(stack2)-1].Val)
        stack2 = stack2[:len(stack2)-1]
    }
    
    return result
}
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
def postorder_traversal(root)
    return [] if root.nil?
    
    stack1 = [root]
    stack2 = []
    result = []
    
    until stack1.empty?
        node = stack1.pop
        stack2.push(node)
        
        stack1.push(node.left) if node.left
        stack1.push(node.right) if node.right
    end
    
    until stack2.empty?
        result.push(stack2.pop.val)
    end
    
    result
end
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
    def postorderTraversal(root: TreeNode): List[Int] = {
        if (root == null) return List()
        
        val stack1 = scala.collection.mutable.Stack[TreeNode]()
        val stack2 = scala.collection.mutable.Stack[TreeNode]()
        stack1.push(root)
        
        while (stack1.nonEmpty) {
            val node = stack1.pop
            stack2.push(node)
            
            if (node.left != null) stack1.push(node.left)
            if (node.right != null) stack1.push(node.right)
        }
        
        stack2.map(_.value).toList
    }
}
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
    pub fn postorder_traversal(root: Option<Rc<RefCell<TreeNode>>>) -> Vec<i32> {
        let mut result = Vec::new();
        if root.is_none() {
            return result;
        }
        
        let mut stack1 = vec![root.unwrap()];
        let mut stack2 = Vec::new();
        
        while let Some(node) = stack1.pop() {
            stack2.push(node.clone());
            if let Some(left) = node.borrow().left.clone() {
                stack1.push(left);
            }
            if let Some(right) = node.borrow().right.clone() {
                stack1.push(right);
            }
        }
        
        while let Some(node) = stack2.pop() {
            result.push(node.borrow().val);
        }
        
        result
    }
}
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

(define/contract (postorder-traversal root)
  (-> (or/c tree-node? #f) (listof exact-integer?))
  
  (define result '())
  
  (define (traverse node)
    (when node
      (traverse (tree-node-left node))
      (traverse (tree-node-right node))
      (set! result (cons (tree-node-val node) result))))
  
  (traverse root)
  (reverse result))
```

### Erlang
```erlang
%% Definition for a binary tree node.
%%
%% -record(tree_node, {val = 0 :: integer(),
%%                     left = null  :: 'null' | #tree_node{},
%%                     right = null :: 'null' | #tree_node{}}).

-spec postorder_traversal(Root :: #tree_node{} | null) -> [integer()].
postorder_traversal(Root) ->
  postorder_traversal(Root, [], []).

postorder_traversal(null, Stack1, Stack2) -> 
    process_stack(Stack1, Stack2);
postorder_traversal(#tree_node{val = Val, left = Left, right = Right}, Stack1, Stack2) ->
    postorder_traversal(Left, [Right|Stack1], [Val|Stack2]).

process_stack([], Result) -> lists:reverse(Result);
process_stack([null|T], Result) -> process_stack(T, Result);
process_stack([H|T], Result) -> 
    postorder_traversal(H, T, Result).
```


### Closing Statement

**Interviewer:** Great job! We have discussed the problem of performing a postorder traversal on a binary tree, explored an initial brute force recursive approach, and then optimized it using an iterative approach with stacks. You implemented and provided solutions in various programming languages, covering the iterative approach effectively. This implementation is solid in terms of time and space complexity, making it robust for typical use cases.

**Interviewee:** Thank you! It was an insightful discussion, and I learned different ways to approach and optimize tree traversal problems. I appreciate the opportunity to explore various languages' implementations as well.

**Interviewer:** You're welcome. Before we wrap up, here are some similar questions that you can practice to further enhance your understanding of tree operations and traversal techniques.

### Similar Questions

1. **Binary Tree Inorder Traversal**
   - Given the `root` of a binary tree, return its nodes' values in inorder traversal.
   - [LeetCode Problem - Binary Tree Inorder Traversal](https://leetcode.com/problems/binary-tree-inorder-traversal/)

2. **Binary Tree Preorder Traversal**
   - Given the `root` of a binary tree, return its nodes' values in preorder traversal.
   - [LeetCode Problem - Binary Tree Preorder Traversal](https://leetcode.com/problems/binary-tree-preorder-traversal/)

3. **Binary Tree Level Order Traversal**
   - Given the `root` of a binary tree, return the level order traversal of its nodes' values. (i.e., from left to right, level by level).
   - [LeetCode Problem - Binary Tree Level Order Traversal](https://leetcode.com/problems/binary-tree-level-order-traversal/)

4. **Binary Tree Zigzag Level Order Traversal**
   - Given the `root` of a binary tree, return the zigzag level order traversal of its nodes' values. (i.e., from left to right, then right to left for the next level and alternate between).
   - [LeetCode Problem - Binary Tree Zigzag Level Order Traversal](https://leetcode.com/problems/binary-tree-zigzag-level-order-traversal/)

5. **Binary Tree Maximum Depth**
   - Given the `root` of a binary tree, return its maximum depth.
   - [LeetCode Problem - Maximum Depth of Binary Tree](https://leetcode.com/problems/maximum-depth-of-binary-tree/)

6. **Symmetric Tree**
   - Given the `root` of a binary tree, check whether it is a mirror of itself (i.e., symmetric around its center).
   - [LeetCode Problem - Symmetric Tree](https://leetcode.com/problems/symmetric-tree/)

These questions will give you more practice with different traversal techniques and other fundamental concepts in binary trees. Good luck with your continued learning and practice!