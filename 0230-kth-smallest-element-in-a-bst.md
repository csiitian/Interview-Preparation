### Interviewer and Interviewee Discussion

**Interviewer:** Let's discuss a problem related to binary search trees. Given the root of a binary search tree (BST) and an integer *k*, I want you to return the *k-th* smallest value (1-indexed) of all the values in the nodes of the tree. How would you approach this problem?

**Interviewee:** Sure. To start off, I think it's important to remember the properties of a binary search tree. In a BST, for each node, all the elements in the left subtree are less than the node's value, and all the elements in the right subtree are greater than the node's value. This property is key to solving this problem efficiently.

**Interviewer:** Great observation. Can you explain your initial thoughts on a brute force approach?

**Interviewee:** Certainly. The brute force approach I can think of involves performing an in-order traversal of the BST. In-order traversal will visit the nodes in ascending order. I can traverse the entire tree and store the values in a list, then simply return the element at index `k-1` from this list.

**Interviewer:** That sounds good. Can you walk me through the implementation details and the complexity of this approach?

### Brute Force Approach

**Interviewee:**

1. Perform an in-order traversal of the tree.
2. Store the resulting node values in an array.
3. Return the (k-1)th element from the array.

Here's a rough code snippet in Python:
```python
def kthSmallest(root, k):
    def in_order_traversal(node, elements):
        if node:
            in_order_traversal(node.left, elements)
            elements.append(node.val)
            in_order_traversal(node.right, elements)
    
    elements = []
    in_order_traversal(root, elements)
    return elements[k-1]
```

**Interviewer:** Nice. What would be the time and space complexity of this approach?

**Interviewee:**

- **Time Complexity:** The in-order traversal takes O(n) time, where n is the number of nodes in the tree.
- **Space Complexity:** Storing the elements in a list also takes O(n) space.

### Optimizing the Solution

**Interviewer:** Your approach is correct, but it can be optimized. Can you think of a way to reduce the space complexity?

**Interviewee:** Yes, we can optimize the approach by not storing all the elements of the tree. Instead, we can keep a counter to count nodes during the in-order traversal. We’ll return the k-th smallest value once our counter reaches k.

Here's a more space-efficient approach:

```python
def kthSmallest(root, k):
    def in_order_traversal(node):
        if not node:
            return None
        
        left = in_order_traversal(node.left)
        if left is not None:
            return left
        
        self.counter += 1
        if self.counter == k:
            return node.val
        
        return in_order_traversal(node.right)
    
    self.counter = 0
    return in_order_traversal(root)
```

**Interviewer:** This definitely reduces the space complexity. What are the time and space complexities of this solution?

**Interviewee:**

- **Time Complexity:** The time complexity remains O(n), since we might still need to visit all nodes in the worst case.
- **Space Complexity:** The space complexity is now O(h), where h is the height of the tree, which corresponds to the recursion stack space. In the worst case, for a skewed tree, this could be O(n).

**Interviewer:** Good. How would you approach the follow-up question if the BST is modified often?

**Interviewee:** For cases where the BST is frequently modified, we could use a data structure that allows for efficient insertion, deletion, and kth smallest element retrieval. One such data structure is an augmented BST where each node maintains a count of the number of nodes in its sub-tree. This allows for O(log n) time complexity on insertions, deletions, and k-th smallest element retrieval.

**Interviewer:** Excellent. Could you summarize and draw a simple example for better understanding?

### Summary and Illustration

**Interviewee:**

1. **Brute Force Approach:**
   - Traverse the tree using in-order traversal and store elements.
   - Return the (k-1)th element from the list.
   - **Time Complexity:** O(n)
   - **Space Complexity:** O(n)

2. **Optimized Approach:**
   - Perform in-order traversal keeping a counter.
   - Return the k-th smallest value once the counter reaches k.
   - **Time Complexity:** O(n)
   - **Space Complexity:** O(h)

**Illustration:**

```
    3
   / \
  1   4
   \
    2
```

For k = 1:
- In-Order Traversal: [1, 2, 3, 4]
- 1st smallest element is 1.

```plaintext
In-order traversal sequence:
1 -> 2 -> 3 -> 4
```

For k = 3:
- In-Order Traversal: [1, 2, 3, 4]
- 3rd smallest element is 3.

```plaintext
In-order traversal sequence:
1 -> 2 -> 3 -> 4
```

### Drawing:

Here's a visualization of the traversal:

```plaintext
    3
   / \
  1   4
   \
    2


In-Order Traversal Steps:

Step 1:
   Visit left subtree of 3 (root)
              3*
             / \
           1*   4
            \
             2
Step 2:
   Visit left subtree of 1
              3
             / \
           1    4
              \
               2*
Step 3:
   Visit 1 itself
              3
             / \
           1*   4
              \
               2
Step 4:
   Visit right subtree of 1
              3
             / \
           1    4
              \
               2*

Step 5:
   Visit 2 itself
              3
             / \
           1    4
              \
               2*
Step 6:
   Visit 3 (root)
              3*
             / \
           1    4
              \
               2
Step 7:
   Visit right subtree of 3
              3
             / \
           1    4*
              \
               2
Step 8:
   Visit 4 itself
              3
             / \
           1    4*
              \
               2
```

In each step, '*' denotes the node currently being visited.

**Interviewer:** Great job! Your explanation was clear, and your approach was solid. Thank you!
Let's implement the optimized solution for each provided language. We will use an in-order traversal, but without storing all node values; instead, we will count nodes as we traverse.

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
    int kthSmallest(TreeNode* root, int k) {
        return inOrder(root, k);
    }

private:
    int inOrder(TreeNode* node, int &k) {
        if (!node) return -1;
        int left = inOrder(node->left, k);
        if (left != -1) return left;
        if (--k == 0) return node->val;
        return inOrder(node->right, k);
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
    public int kthSmallest(TreeNode root, int k) {
        return inOrder(root, k, new int[1]);
    }

    private int inOrder(TreeNode node, int k, int[] count) {
        if (node == null) return -1;
        int left = inOrder(node.left, k, count);
        if (left != -1) return left;
        count[0]++;
        if (count[0] == k) return node.val;
        return inOrder(node.right, k, count);
    }
}
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
    def kthSmallest(self, root, k):
        """
        :type root: TreeNode
        :type k: int
        :rtype: int
        """
        def in_order_traversal(node):
            if node:
                left = in_order_traversal(node.left)
                if left is not None:
                    return left
                self.counter += 1
                if self.counter == k:
                    return node.val
                return in_order_traversal(node.right)
        
        self.counter = 0
        return in_order_traversal(root)
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
    def kthSmallest(self, root: Optional[TreeNode], k: int) -> int:
        def in_order_traversal(node):
            if node:
                left = in_order_traversal(node.left)
                if left is not None:
                    return left
                self.counter += 1
                if self.counter == k:
                    return node.val
                return in_order_traversal(node.right)
        
        self.counter = 0
        return in_order_traversal(root)
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
int inOrder(struct TreeNode* node, int* k) {
    if (!node) return -1;
    int left = inOrder(node->left, k);
    if (left != -1) return left;
    if (--(*k) == 0) return node->val;
    return inOrder(node->right, k);
}

int kthSmallest(struct TreeNode* root, int k) {
    return inOrder(root, &k);
}
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
    public int KthSmallest(TreeNode root, int k) {
        return InOrder(root, ref k);
    }

    private int InOrder(TreeNode node, ref int k) {
        if (node == null) return -1;
        int left = InOrder(node.left, ref k);
        if (left != -1) return left;
        if (--k == 0) return node.val;
        return InOrder(node.right, ref k);
    }
}
```

### JavaScript

```javascript
/**
 * Definition for a binary tree node.
 * function TreeNode(val, left, right) {
 *     this.val = (val===undefined ? 0 : val);
 *     this.left = (left===undefined ? null : left);
 *     this.right = (right===undefined ? null : right);
 * }
 */
/**
 * @param {TreeNode} root
 * @param {number} k
 * @return {number}
 */
var kthSmallest = function(root, k) {
    function inOrder(node) {
        if (!node) return -1;
        let left = inOrder(node.left);
        if (left !== -1) return left;
        if (--k === 0) return node.val;
        return inOrder(node.right);
    }
    return inOrder(root);
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

function kthSmallest(root: TreeNode | null, k: number): number {
    function inOrder(node: TreeNode | null): number {
        if (!node) return -1;
        let left = inOrder(node.left);
        if (left !== -1) return left;
        if (--k === 0) return node.val;
        return inOrder(node.right);
    }
    return inOrder(root);
}
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
     * @param Integer $k
     * @return Integer
     */
    function kthSmallest($root, $k) {
        return $this->inOrder($root, $k);
    }

    private function inOrder($node, &$k) {
        if (!$node) return null;
        $left = $this->inOrder($node->left, $k);
        if ($left !== null) return $left;
        if (--$k == 0) return $node->val;
        return $this->inOrder($node->right, $k);
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
    func kthSmallest(_ root: TreeNode?, _ k: Int) -> Int {
        var count = k
        return inOrder(root, &count)
    }

    private func inOrder(_ node: TreeNode?, _ k: inout Int) -> Int {
        guard let node = node else { return -1 }
        let left = inOrder(node.left, &k)
        if left != -1 { return left }
        k -= 1
        if k == 0 { return node.val }
        return inOrder(node.right, &k)
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
    fun kthSmallest(root: TreeNode?, k: Int): Int {
        return inOrder(root, intArrayOf(k))
    }

    private fun inOrder(node: TreeNode?, k: IntArray): Int {
        if (node == null) return -1
        val left = inOrder(node.left, k)
        if (left != -1) return left
        k[0]--
        if (k[0] == 0) return node.`val`
        return inOrder(node.right, k)
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
  int kthSmallest(TreeNode? root, int k) {
    return _inOrder(root, List.filled(1, k));
  }

  int _inOrder(TreeNode? node, List<int> k) {
    if (node == null) return -1;
    int left = _inOrder(node.left, k);
    if (left != -1) return left;
    k[0]--;
    if (k[0] == 0) return node.val;
    return _inOrder(node.right, k);
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
func kthSmallest(root *TreeNode, k int) int {
    return inOrder(root, &k)
}

func inOrder(node *TreeNode, k *int) int {
    if node == nil {
        return -1
    }
    left := inOrder(node.Left, k)
    if left != -1 {
        return left
    }
    *k--
    if *k == 0 {
        return node.Val
    }
    return inOrder(node.Right, k)
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
# @param {Integer} k
# @return {Integer}
def kth_smallest(root, k)
  return in_order(root, k, [-1])
end

def in_order(node, k, count)
  return -1 if node.nil?
  left = in_order(node.left, k, count)
  return left unless left == -1
  count[0] += 1
  return node.val if count[0] == k
  return in_order(node.right, k, count)
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
    def kthSmallest(root: TreeNode, k: Int): Int = {
        var count = k
        return inOrder(root, count)
    }

    def inOrder(node: TreeNode, k: Int): Int = {
        if (node == null) return -1
        val left = inOrder(node.left, k)
        if (left != -1) return left
        k -= 1
        if (k == 0) return node.value
        return inOrder(node.right, k)
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
    pub fn kth_smallest(root: Option<Rc<RefCell<TreeNode>>>, k: i32) -> i32 {
        Self::in_order(root, k, &mut 0)
    }

    fn in_order(node: Option<Rc<RefCell<TreeNode>>>, k: i32, counter: &mut i32) -> i32 {
        if let Some(node) = node {
            let left = Self::in_order(node.borrow().left.clone(), k, counter);
            if left != -1 {
                return left;
            }
            *counter += 1;
            if *counter == k {
                return node.borrow().val;
            }
            return Self::in_order(node.borrow().right.clone(), k, counter);
        }
        -1
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


(define/contract (kth-smallest root k)
  (-> (or/c tree-node? #f) exact-integer? exact-integer?)
  (define (in-order node k)
    (if (null? node) -1
      (let ((left (in-order (tree-node-left node) k)))
        (if (not (= left -1)) left
          (begin0
            (tree-node-val node)
            (set! k (- k 1))
            (if (= k 0)
                (tree-node-val node)
                (in-order (tree-node-right node) k)))))))
  (in-order root k))
```

### Erlang

```erlang
%% Definition for a binary tree node.
%%
%% -record(tree_node, {val = 0 :: integer(),
%%                     left = null  :: 'null' | #tree_node{},
%%                     right = null :: 'null' | #tree_node{}}).

-spec kth_smallest(Root :: #tree_node{} | null, K :: integer()) -> integer().
kth_smallest(Root, K) ->
    in_order(Root, K, 0).

-spec in_order(#tree_node{} | null, integer(), integer()) -> integer().
in_order(null, _, _) ->
    -1;
in_order(#tree_node{val = Val, left = L, right = R}, K, Count) ->
    Left = in_order(L, K, Count),
    if
        Left =:= -1 ->
            NewCount = Count + 1,
            if
                NewCount =:= K -> 
                    Val;
                true ->
                    in_order(R, K, NewCount)
            end;
        true -> Left
    end.
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
  @spec kth_smallest(root :: TreeNode.t | nil, k :: integer) :: integer
  def kth_smallest(root, k) do
    in_order(root, k, [0])
  end

  defp in_order(nil, _k, _count), do: -1

  defp in_order(%TreeNode{val: val, left: left, right: right}, k, counter) do
    left_res = in_order(left, k, counter)
    if left_res != -1, do: left_res, else:
      updated_counter = counter ++ [hd(counter) + 1]
      if hd(updated_counter) == k do
        val
      else
        in_order(right, k, updated_counter)
      end
  end
end
```

The optimized solution in each language leverages the properties of BSTs and efficient in-order traversal to find the k-th smallest element with minimal space complexity.


### Closing Statement

In our discussion, we delved into the problem of finding the k-th smallest element in a Binary Search Tree (BST). Starting with an intuitive brute force approach, we optimized our solution to be more space-efficient by utilizing in-order traversal without the need for additional storage. We explored implementations across a wide array of programming languages, demonstrating the versatility and adaptability of the algorithm to different paradigms and syntax rules.

By leveraging in-order traversal, we efficiently maintained the BST properties and minimized space complexity while ensuring that the time complexity remains linear relative to the number of nodes. Additionally, this solution scales well given the constraints typically associated with such problems, making it both practical and performant.

This discussion and the ensuing implementations offer a robust solution to a common BST problem, exemplifying the importance of understanding tree traversal techniques and the nuances of each programming language.

### Similar Questions

Here are some related questions that further explore BSTs and tree traversal techniques:

1. **Find the Largest Element in a BST:** Given the root of a BST, find the largest element in the tree.
2. **Find the k-th Largest Element in a BST:** Similar to our discussion but finding the k-th largest element instead.
3. **Validate Binary Search Tree:** Given the root of a tree, determine if it is a valid BST.
4. **Convert Sorted Array to Binary Search Tree:** Given a sorted array, convert it into a height-balanced BST.
5. **Lowest Common Ancestor in a BST:** Find the lowest common ancestor of two nodes in a BST.
6. **Range Sum of BST:** Given the root of a BST and a range [L, R], compute the sum of values of nodes within the range.
7. **In-order Successor in BST:** Given a node in a BST, find its in-order successor.
8. **Construct BST from Preorder Traversal:** Construct a BST from its preorder traversal.
9. **Closest Binary Search Tree Value:** Given a target value, find the value in the BST that is closest to the target.

These questions further challenge one's understanding of BST properties, tree traversal techniques, and algorithmic efficiency, thus offering a comprehensive practice set for mastering concepts related to binary search trees.