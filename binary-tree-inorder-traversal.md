### Interviewer and Interviewee Discussion

**Interviewer:** Let's discuss the problem. You need to return the inorder traversal of the nodes' values from a given binary tree. How would you start approaching this problem?

**Interviewee:** Sure. Inorder traversal of a binary tree implies visiting the nodes in the order: left child, root, and then right child. So for the given examples, we need to traverse the tree in such a way.

**Interviewer:** Correct. What would be your initial thoughts on solving this problem? Can you describe a brute force approach?

**Interviewee:** The brute force approach would be to use recursion. We can create a helper function that takes a node and performs the following steps:
1. Recursively call the helper function for the left child (if present).
2. Append the value of the current node to the result list.
3. Recursively call the helper function for the right child (if present).

### Brute Force Approach

**Interviewer:** Alright, walk me through the code for this recursive solution.

**Interviewee:**
```python
def inorderTraversal(root):
    def inorder(node, result):
        if node is not None:
            inorder(node.left, result)   # Visit left subtree
            result.append(node.val)      # Visit node itself
            inorder(node.right, result)  # Visit right subtree

    result = []
    inorder(root, result)
    return result
```

**Interviewer:** That makes sense. What can you say about the time and space complexity of this approach?

**Interviewee:** 
- **Time Complexity:** O(n), where n is the number of nodes in the binary tree. Each node is visited exactly once.
- **Space Complexity:** O(n) in the worst case due to the recursion stack (if the tree is completely unbalanced, i.e., a linear chain).

**Interviewer:** Those are valid points. Now, the follow-up question asks if we can perform this inorder traversal iteratively. How might you go about optimizing the solution using an iterative approach?

**Interviewee:** For the iterative approach, we can use a stack to simulate the recursive behavior. Here’s how it can be done:
1. Use a stack to keep track of nodes.
2. Start with the root node and push it onto the stack.
3. Go as left as possible, pushing all the nodes onto the stack until you hit a leaf.
4. Pop an item from the stack, process it, and then move to the right child.
5. Repeat steps 3 and 4 until the stack is empty and all nodes are processed.

### Iterative Solution

**Interviewer:** Could you provide a code implementation for this iterative approach?

**Interviewee:**

```python
def inorderTraversal(root):
    result = []
    stack = []
    current = root

    while current is not None or stack:
        while current is not None:
            stack.append(current)
            current = current.left

        current = stack.pop()
        result.append(current.val)
        current = current.right

    return result
```

### Explanation with Illustration

**Interviewee:** Let me draw an example to explain this better:

Consider a simple binary tree:
```
    1
     \
      2
     /
    3
```

1. Start with root node 1. Push to stack.
   Stack: [1]
   
2. Move to the left child of 1, which is null. 
   Stack: [1] (no left child)
   
3. Pop from stack and add to result. Move to the right child.
   Stack: []
   Result: [1]
   Current node: 2
   
4. Repeat the steps for node 2.
    - Push to stack
    - Move to the left (node 3)
    - Stack: [2]
    - Current: 3
    
5. Move to left child of 3 (null)
   Stack: [2, 3]
   
6. Pop 3 from stack and move to the right of 3.
   Result: [1, 3]
   Stack: [2]
   Current: null
   
7. Pop 2 from stack and move to the right of 2.
   Result: [1, 3, 2]
   Stack: []
   
The final result is [1, 3, 2].

**Interviewer:** Great! This iterative solution efficiently uses a stack to simulate the recursive traversal, while maintaining an O(n) time complexity and O(n) space complexity for the stack usage. Well done!
Sure! I'll provide code snippets for the iterative inorder traversal solution for each of the given programming languages along with time and space complexity comments.

### C++

```cpp
/**
 * Definition for a binary tree node.
 * struct TreeNode {
 *     int val;
 *     TreeNode *left;
 *     TreeNode *right;
 *     TreeNode() : val(0), left(nullptr), right(nullptr) {}
 *     TreeNode(int x) : val(x), left(nullptr, right(nullptr)) {}
 *     TreeNode(int x, TreeNode *left, TreeNode *right) : val(x), left(left), right(right) {}
 * };
 */
class Solution {
public:
    vector<int> inorderTraversal(TreeNode* root) {
        vector<int> result;
        stack<TreeNode*> stack;
        TreeNode* current = root;
        
        while (current != nullptr || !stack.empty()) {
            while (current != nullptr) {
                stack.push(current);
                current = current->left;
            }
            current = stack.top();
            stack.pop();
            result.push_back(current->val);
            current = current->right;
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
class Solution {
    public List<Integer> inorderTraversal(TreeNode root) {
        List<Integer> result = new ArrayList<>();
        Stack<TreeNode> stack = new Stack<>();
        TreeNode current = root;
        
        while (current != null || !stack.isEmpty()) {
            while (current != null) {
                stack.push(current);
                current = current.left;
            }
            current = stack.pop();
            result.add(current.val);
            current = current.right;
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
    def inorderTraversal(self, root):
        """
        :type root: TreeNode
        :rtype: List[int]
        """
        result = []
        stack = []
        current = root
        
        while current or stack:
            while current:
                stack.append(current)
                current = current.left
            current = stack.pop()
            result.append(current.val)
            current = current.right
        
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
    def inorderTraversal(self, root: Optional[TreeNode]) -> List[int]:
        result = []
        stack = []
        current = root
        
        while current or stack:
            while current:
                stack.append(current)
                current = current.left
            current = stack.pop()
            result.append(current.val)
            current = current.right
        
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
int* inorderTraversal(struct TreeNode* root, int* returnSize) {
    int* result = (int*)malloc(100 * sizeof(int));
    struct TreeNode* stack[100];
    int stackSize = 0;
    int* top = result;
   
    *returnSize = 0;
    
    struct TreeNode* current = root;
    
    while (current != NULL || stackSize > 0) {
        while (current != NULL) {
            stack[stackSize++] = current;
            current = current->left;
        }
        current = stack[--stackSize];
        *top++ = current->val;
        (*returnSize)++;
        current = current->right;
    }
    
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
public class Solution {
    public IList<int> InorderTraversal(TreeNode root) {
        List<int> result = new List<int>();
        Stack<TreeNode> stack = new Stack<TreeNode>();
        TreeNode current = root;
        
        while (current != null || stack.Count > 0) {
            while (current != null) {
                stack.Push(current);
                current = current.left;
            }
            current = stack.Pop();
            result.Add(current.val);
            current = current.right;
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
var inorderTraversal = function(root) {
    const result = [];
    const stack = [];
    let current = root;
    
    while (current !== null || stack.length > 0) {
        while (current !== null) {
            stack.push(current);
            current = current.left;
        }
        current = stack.pop();
        result.push(current.val);
        current = current.right;
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

function inorderTraversal(root: TreeNode | null): number[] {
    const result: number[] = [];
    const stack: TreeNode[] = [];
    let current: TreeNode | null = root;
    
    while (current !== null || stack.length > 0) {
        while (current !== null) {
            stack.push(current);
            current = current.left;
        }
        current = stack.pop() as TreeNode;
        result.push(current.val);
        current = current.right;
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
    function inorderTraversal($root) {
        $result = [];
        $stack = [];
        $current = $root;
        
        while ($current !== null || !empty($stack)) {
            while ($current !== null) {
                $stack[] = $current;
                $current = $current->left;
            }
            $current = array_pop($stack);
            $result[] = $current->val;
            $current = $current->right;
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
    func inorderTraversal(_ root: TreeNode?) -> [Int] {
        var result = [Int]()
        var stack = [TreeNode]()
        var current = root
        
        while current != nil || !stack.isEmpty {
            while current != nil {
                stack.append(current!)
                current = current!.left
            }
            current = stack.removeLast()
            result.append(current!.val)
            current = current!.right
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
    fun inorderTraversal(root: TreeNode?): List<Int> {
        val result = mutableListOf<Int>()
        val stack = mutableListOf<TreeNode>()
        var current = root
        
        while (current != null || stack.isNotEmpty()) {
            while (current != null) {
                stack.add(current)
                current = current.left
            }
            current = stack.removeAt(stack.size - 1)
            result.add(current.`val`)
            current = current.right
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
  List<int> inorderTraversal(TreeNode? root) {
    List<int> result = [];
    List<TreeNode> stack = [];
    TreeNode? current = root;
    
    while (current != null || stack.isNotEmpty) {
      while (current != null) {
        stack.add(current);
        current = current.left;
      }
      current = stack.removeLast();
      result.add(current.val);
      current = current.right;
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
func inorderTraversal(root *TreeNode) []int {
    var result []int
    var stack []*TreeNode
    var current = root
    
    for current != nil || len(stack) > 0 {
        for current != nil {
            stack = append(stack, current)
            current = current.Left
        }
        current = stack[len(stack) - 1]
        stack = stack[:len(stack) - 1]
        result = append(result, current.Val)
        current = current.Right
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
def inorder_traversal(root)
    result = []
    stack = []
    current = root
    
    while current || !stack.empty?
        while current
            stack.push(current)
            current = current.left
        end
        current = stack.pop
        result.push(current.val)
        current = current.right
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
    def inorderTraversal(root: TreeNode): List[Int] = {
        import scala.collection.mutable.Stack
        val result = ListBuffer[Int]()
        val stack = Stack[TreeNode]()
        var current: TreeNode = root
        
        while (current != null || stack.nonEmpty) {
            while (current != null) {
                stack.push(current)
                current = current.left
            }
            current = stack.pop()
            result += current.value
            current = current.right
        }
        
        result.toList
    }
}

// Time Complexity: O(n)
// Space Complexity: O(n)
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
    pub fn inorder_traversal(root: Option<Rc<RefCell<TreeNode>>>) -> Vec<i32> {
        let mut result = Vec::new();
        let mut stack = Vec::new();
        let mut current = root;
        
        while current.is_some() || !stack.is_empty() {
            while let Some(node) = current {
                stack.push(node.clone());
                current = node.borrow().left.clone();
            }
            current = stack.pop();
            if let Some(node) = current {
                result.push(node.borrow().val);
                current = node.borrow().right.clone();
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

(define/contract (inorder-traversal root)
  (-> (or/c tree-node? #f) (listof exact-integer?))
  (define result '())
  (define stack '())
  (define current root)
  
  (define (loop)
    (cond [(or current (not (null? stack)))
           (if current
               (begin
                 (set! stack (cons current stack))
                 (set! current (tree-node-left current)))
               (begin
                 (set! current (car stack))
                 (set! stack (cdr stack))
                 (set! result (append result (list (tree-node-val current))))
                 (set! current (tree-node-right current)))
           (loop)])
          [else result]))
  (loop))

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

-spec inorder_traversal(Root :: #tree_node{} | null) -> [integer()].
inorder_traversal(Root) ->
    inorder_traversal(Root, [], []).

inorder_traversal(null, Stack, Result) ->
    reverse(Result);
inorder_traversal(#tree_node{} = Node, Stack, Result) ->
    inorder_traversal(Node#tree_node.left, [Node | Stack], Result);
inorder_traversal([], [Top | RestStack], Result) ->
    inorder_traversal(Top#tree_node.right, RestStack, [Top#tree_node.val | Result]).

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
  @spec inorder_traversal(root :: TreeNode.t() | nil) :: [integer]
  def inorder_traversal(root) do
    inorder_traversal(root, [], [])
  end

  defp inorder_traversal(nil, [], result), do: Enum.reverse(result)
  defp inorder_traversal(nil, [top | rest_stack], result) do
    inorder_traversal(top.right, rest_stack, [top.val | result])
  end
  defp inorder_traversal(%TreeNode{} = node, stack, result) do
    inorder_traversal(node.left, [node | stack], result)
  end
end

# Time Complexity: O(n)
# Space Complexity: O(n)
```

These solutions all provide iterative inorder traversal implementations with the associated time and

### Closing Statement

Thank you for the comprehensive discussion on the inorder traversal of a binary tree. We started by examining a straightforward recursive approach to understand the underlying structure of the problem. From there, we optimized our solution to an iterative approach using a stack to simulate the recursion, thereby adhering to the follow-up constraint of achieving an iterative solution. By meticulously understanding the problem and exploring the iterative solution, we ensured that our approach remained efficient with a time complexity of O(n) and a space complexity of O(n).

We also translated the solution into multiple programming languages, demonstrating the versatility and adaptability of our algorithm. This exercise reinforced our understanding of the iterative inorder traversal and illustrated how such algorithms can be effectively implemented across different coding environments.

### Similar Questions

Here are some similar questions that you can practice to further reinforce your understanding of tree traversal algorithms:

1. **Binary Tree Preorder Traversal**:
    - Given the root of a binary tree, return the preorder traversal of its nodes' values (Root -> Left -> Right).

2. **Binary Tree Postorder Traversal**:
    - Given the root of a binary tree, return the postorder traversal of its nodes' values (Left -> Right -> Root).

3. **Binary Tree Level Order Traversal**:
    - Given the root of a binary tree, return the level order traversal of its nodes' values. (i.e., from left to right, level by level).

4. **Binary Search Tree Insertion**:
    - Given the root of a binary search tree (BST) and a value to be inserted into the tree, insert the value such that the tree maintains the properties of a BST.

5. **Binary Tree Zigzag Level Order Traversal**:
    - Given the root of a binary tree, return the zigzag level order traversal of its nodes' values. (i.e., left to right, then right to left for the next level and alternate between).

6. **Lowest Common Ancestor of a Binary Search Tree**:
    - Given a binary search tree (BST), find the lowest common ancestor (LCA) of two given nodes in the BST.

7. **Validate Binary Search Tree**:
    - Given the root of a binary tree, determine if it is a valid binary search tree (BST).

8. **Flatten Binary Tree to Linked List**:
    - Given the root of a binary tree, flatten the tree into a "linked list":
      - The "linked list" should be in the same order as a pre-order traversal of the binary tree.

By practicing these related problems, you can deepen your understanding of binary tree traversals, manipulations, and properties, and further enhance your problem-solving skills in this domain. Happy coding!