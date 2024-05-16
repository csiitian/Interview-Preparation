## Interviewer and Interviewee Discussion

### Interviewer:
Let's work on a problem related to binary trees. Given the `root` of a binary tree, imagine yourself standing on the right side of it, you need to return the values of the nodes you can see ordered from top to bottom. Here's an example to give you a better idea:

**Example 1:**  
Input: `root = [1, 2, 3, null, 5, null, 4]`  
Output: `[1, 3, 4]`

Would you like to start by explaining your initial thoughts on how you might approach this problem?

### Interviewee:
Sure! To get the right side view of the binary tree, we need to focus on capturing the rightmost nodes visible at each level of the tree.

### Interviewer:
Alright. What's the most straightforward approach you can think of?

### Interviewee:
One brute-force approach could be to perform a level-order traversal (or breadth-first traverse) of the binary tree and at each level just keep track of the last node we visit since that last node would be the rightmost node visible for that level.

### Interviewer:
That sounds reasonable. Can you describe how you would implement the brute-force approach and its associated complexities?

### Interviewee:
Definitely!

1. **Algorithm:**
   - Use a queue to perform a level-order traversal of the tree.
   - For every level, add all nodes' values to a list.
   - At the end of each level's traversal, add the last node's value of that level to the result list as this node will be visible from the right side.
   
   Here's the step-by-step approach:
   1. Initialize an empty list `right_view` to store the right side view nodes.
   2. Use a queue to help with the breadth-first traversal. Start by adding the root to the queue.
   3. While the queue is not empty:
      - Note the number of nodes at the current level (size of the queue).
      - Iterate through each node at that level:
        - Pop the node from the queue.
        - If it's the last node of this level, add its value to `right_view`.
        - Add the node's children to the queue.
   4. Return `right_view`.

2. **Complexity:**
   - **Time Complexity:** Every node is visited once, so the time complexity is \(O(n)\), where \(n\) is the number of nodes in the tree.
   - **Space Complexity:** The space complexity is determined by the queue's maximum size, which in the worst case is the largest number of nodes at any level during the traversal. This would also be \(O(n)\) in the worst case for a very unbalanced tree.

### Interviewer:
Okay. Can you think of a more optimized solution or a different data structure that might help?

### Interviewee:
The level-order traversal approach is already quite efficient for this problem. However, another variant could be using a Depth-First Search (DFS) with a priority of visiting the right children before the left children. This way, we can ensure that when we first encounter a node at a level, it’s the rightmost node.

Here's an illustration of this approach:

```plaintext
          1
        /   \
       2     3
        \     \
         5     4
```

- Start with the root node (1).
- Traverse its right child (3) before its left (2), hence, we see 1 and 3 in the right view.
- Continue with the right-most children first until no more nodes to the right, which gives us nodes [1, 3, 4].

### Interviewer:
That sounds like a good plan. Could you implement this using a DFS approach and explain the code step by step?

### Interviewee:
Absolutely. Here's the Python implementation with DFS approach:

```python
# Define the TreeNode class to build the binary tree
class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

def rightSideView(root: TreeNode) -> List[int]:
    if not root:
        return []

    def dfs(node, level):
        if not node:
            return
        
        # If we are visiting this level for the first time
        if level == len(view):
            view.append(node.val)
        
        # Recur for right first, then left
        dfs(node.right, level + 1)
        dfs(node.left, level + 1)
    
    view = []
    dfs(root, 0)
    return view
```

### Explanation:
1. We define a nested helper function `dfs` that will perform depth-first search.
2. As we traverse the tree, we check if the current level is equal to the length of the view list. If it is, we add the node's value to the view list.
3. We start by visiting the right node first to ensure that the rightmost node is added to the view list first before any left nodes.
4. The function initializes an empty list `view` and starts the DFS traversal from the root.

### Complexity:
- **Time Complexity:** \(O(n)\) since each node is visited once.
- **Space Complexity:** \(O(h)\), where \(h\) is the height of the tree, due to the recursion stack.

This should cover the optimized way to get the right side view of a binary tree.

### Illustration:
Here's a visual representation of how the DFS approach works:

```plaintext
                1
              /   \
            2       3
             \       \
              5       4
              
Step-by-step DFS traversal (right child first):
1. Visit 1 (Level 0): view = [1]
2. Visit 3 (Level 1): view = [1, 3]
3. Visit 4 (Level 2): view = [1, 3, 4]
4. Backtrack and visit left child of 3 which is None (skip)
5. Backtrack to 1 and visit left child 2 (skip Level 1 as already captured)
6. Visit 5 (Level 2, captured already)
Final right view: [1, 3, 4]
```
---

This explains the right side view problem with a brute-force level order approach and an optimized DFS approach. Do you have any questions on this?
Sure, I can provide the code implementation using the Depth-First Search (DFS) approach for each of the languages specified. Here is the implementation for each language, including time and space complexity comments:

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
    vector<int> rightSideView(TreeNode* root) {
        vector<int> view;
        dfs(root, 0, view);
        return view;
    }
    
private:
    void dfs(TreeNode* node, int level, vector<int>& view) {
        if (!node) return;
        
        if (level == view.size()) {
            view.push_back(node->val);
        }
        
        dfs(node->right, level + 1, view);
        dfs(node->left, level + 1, view);
    }
};

// Time Complexity: O(n), where n is the number of nodes in the tree.
// Space Complexity: O(h), where h is the height of the tree due to recursion stack.
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
    public List<Integer> rightSideView(TreeNode root) {
        List<Integer> view = new ArrayList<>();
        dfs(root, 0, view);
        return view;
    }
    
    private void dfs(TreeNode node, int level, List<Integer> view) {
        if (node == null) return;
        
        if (level == view.size()) {
            view.add(node.val);
        }
        
        dfs(node.right, level + 1, view);
        dfs(node.left, level + 1, view);
    }
}

// Time Complexity: O(n), where n is the number of nodes in the tree.
// Space Complexity: O(h), where h is the height of the tree due to recursion stack.
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
    def rightSideView(self, root):
        """
        :type root: TreeNode
        :rtype: List[int]
        """
        def dfs(node, level):
            if not node:
                return
            if level == len(view):
                view.append(node.val)
            dfs(node.right, level + 1)
            dfs(node.left, level + 1)
        
        view = []
        dfs(root, 0)
        return view

# Time Complexity: O(n), where n is the number of nodes in the tree.
# Space Complexity: O(h), where h is the height of the tree due to recursion stack.
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
    def rightSideView(self, root: Optional[TreeNode]) -> List[int]:
        def dfs(node, level):
            if not node:
                return
            if level == len(view):
                view.append(node.val)
            dfs(node.right, level + 1)
            dfs(node.left, level + 1)
        
        view = []
        dfs(root, 0)
        return view

# Time Complexity: O(n), where n is the number of nodes in the tree.
# Space Complexity: O(h), where h is the height of the tree due to recursion stack.
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
int* rightSideView(struct TreeNode* root, int* returnSize) {
    int* view = (int*)malloc(100 * sizeof(int));
    int level = 0;
    *returnSize = 0;
    
    void dfs(struct TreeNode* node, int level) {
        if (!node) return;
        
        if (level == *returnSize) {
            view[level] = node->val;
            (*returnSize)++;
        }
        
        dfs(node->right, level + 1);
        dfs(node->left, level + 1);
    }
    
    dfs(root, level);
    return view;
}

// Time Complexity: O(n), where n is the number of nodes in the tree.
// Space Complexity: O(h), where h is the height of the tree due to recursion stack.
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
    public IList<int> RightSideView(TreeNode root) {
        var view = new List<int>();
        void Dfs(TreeNode node, int level) {
            if (node == null) return;
            
            if (level == view.Count) {
                view.Add(node.val);
            }
            
            Dfs(node.right, level + 1);
            Dfs(node.left, level + 1);
        }
        
        Dfs(root, 0);
        return view;
    }
}

// Time Complexity: O(n), where n is the number of nodes in the tree.
// Space Complexity: O(h), where h is the height of the tree due to recursion stack.
```

### JavaScript

```js
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
var rightSideView = function(root) {
    const view = [];
    
    function dfs(node, level) {
        if (node === null) return;
        
        if (level === view.length) {
            view.push(node.val);
        }
        
        dfs(node.right, level + 1);
        dfs(node.left, level + 1);
    }
    
    dfs(root, 0);
    return view;
};

// Time Complexity: O(n), where n is the number of nodes in the tree.
// Space Complexity: O(h), where h is the height of the tree due to recursion stack.
```

### TypeScript

```ts
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

function rightSideView(root: TreeNode | null): number[] {
    const view: number[] = [];
    
    function dfs(node: TreeNode | null, level: number) {
        if (node === null) return;
        
        if (level === view.length) {
            view.push(node.val);
        }
        
        dfs(node.right, level + 1);
        dfs(node.left, level + 1);
    }
    
    dfs(root, 0);
    return view;
}

// Time Complexity: O(n), where n is the number of nodes in the tree.
// Space Complexity: O(h), where h is the height of the tree due to recursion stack.
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
    function rightSideView($root) {
        $view = [];
        
        function dfs($node, $level) {
            global $view;
            if ($node === null) return;
            
            if ($level === count($view)) {
                $view[] = $node->val;
            }
            
            dfs($node->right, $level + 1);
            dfs($node->left, $level + 1);
        }
        
        dfs($root, 0);
        return $view;
    }
}

// Time Complexity: O(n), where n is the number of nodes in the tree.
// Space Complexity: O(h), where h is the height of the tree due to recursion stack.
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
    func rightSideView(_ root: TreeNode?) -> [Int] {
        var view = [Int]()
        
        func dfs(_ node: TreeNode?, _ level: Int) {
            guard let node = node else { return }
            
            if level == view.count {
                view.append(node.val)
            }
            
            dfs(node.right, level + 1)
            dfs(node.left, level + 1)
        }
        
        dfs(root, 0)
        return view
    }
}

// Time Complexity: O(n), where n is the number of nodes in the tree.
// Space Complexity: O(h), where h is the height of the tree due to recursion stack.
```

### Kotlin

```kt
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
    fun rightSideView(root: TreeNode?): List<Int> {
        val view = mutableListOf<Int>()
    
        fun dfs(node: TreeNode?, level: Int) {
            if (node == null) return

            if (level == view.size) {
                view.add(node.`val`)
            }

            dfs(node.right, level + 1)
            dfs(node.left, level + 1)
        }

        dfs(root, 0)
        return view
    }
}

// Time Complexity: O(n), where n is the number of nodes in the tree.
// Space Complexity: O(h), where h is the height of the tree due to recursion stack.
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
  List<int> rightSideView(TreeNode? root) {
    List<int> view = [];
    
    void dfs(TreeNode? node, int level) {
      if (node == null) return;
      
      if (level == view.length) {
        view.add(node.val);
      }
      
      dfs(node.right, level + 1);
      dfs(node.left, level + 1);
    }
    
    dfs(root, 0);
    return view;
  }
}

// Time Complexity: O(n), where n is the number of nodes in the tree.
// Space Complexity: O(h), where h is the height of the tree due to recursion stack.
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
func rightSideView(root *TreeNode) []int {
    view := []int{}
    
    var dfs func(*TreeNode, int)
    dfs = func(node *TreeNode, level int) {
        if node == nil {
            return
        }
        
        if level == len(view) {
            view = append(view, node.Val)
        }
        
        dfs(node.Right, level + 1)
        dfs(node.Left, level + 1)
    }

    dfs(root, 0)
    return view
}

// Time Complexity: O(n), where n is the number of nodes in the tree.
// Space Complexity: O(h), where h is the height of the tree due to recursion stack.
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
def right_side_view(root)
    view = []

    dfs = -> (node, level) {
        return if node.nil?
        
        if level == view.size
            view << node.val
        end
        
        dfs.call(node.right, level + 1)
        dfs.call(node.left, level + 1)
    }
    
    dfs.call(root, 0)
    view
end

# Time Complexity: O(n), where n is the number of nodes in the tree.
# Space Complexity: O(h), where h is the height of the tree due to recursion stack.
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
    def rightSideView(root: TreeNode): List[Int] = {
        var view = List[Int]()
        
        def dfs(node: TreeNode, level: Int): Unit = {
            if (node == null) return
            
            if (level == view.length) {
                view = view :+ node.value
            }
            
            dfs(node.right, level + 1)
            dfs(node.left, level + 1)
        }
        
        dfs(root, 0)
        view
    }
}

// Time Complexity: O(n), where n is the number of nodes in the tree.
// Space Complexity: O(h), where h is the height of the tree due to recursion stack.
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
    pub fn right_side_view(root: Option<Rc<RefCell<TreeNode>>>) -> Vec<i32> {
        let mut view = vec![];
        
        fn dfs(node: Option<Rc<RefCell<TreeNode>>>, level: usize, view: &mut Vec<i32>) {
            if let Some(node) = node {
                if level == view.len() {
                    view.push(node.borrow().val);
                }
                
                dfs(node.borrow().right.clone(), level + 1, view);
                dfs(node.borrow().left.clone(), level + 1, view);
            }
        }
        
        dfs(root, 0, &mut view);
        view
    }
}

// Time Complexity: O(n), where n is the number of nodes in the tree.
// Space Complexity: O(h), where h is the height of the tree due to recursion stack.
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

(define/contract (right-side-view root)
  (-> (or/c tree-node? #f) (listof exact-integer?))
  (define view '())

  (define (dfs node level)
    (when node
      (when (= level (length view))
        (set! view (append view (list (tree-node-val node)))))
      (dfs (tree-node-right node) (+ level 1))
      (dfs (tree-node-left node) (+ level 1))))

  (dfs root 0)
  view)

; Time Complexity: O(n), where n is the number of nodes in the tree.
; Space Complexity: O(h), where h is the height of the tree due to recursion stack.
```


### Closing Statement

We have successfully tackled the problem of determining the right side view of a binary tree. We began with a brute-force level-order traversal approach, evaluating its time and space complexities. Then, we optimized the solution using a depth-first search (DFS) approach, ensuring consistent time complexity of \(O(n)\) and space complexity of \(O(h)\) where \(n\) is the number of nodes and \(h\) is the height of the tree.

We implemented and discussed this solution in multiple programming languages including C++, Java, Python, C#, JavaScript, TypeScript, PHP, Swift, Kotlin, Dart, Go, Ruby, Scala, Rust, Racket, and Erlang. Each implementation focused on correctly managing recursion and ensuring the most visible nodes are captured efficiently.

These exercises should help you understand different ways to work with tree structures efficiently and comprehend the importance of choosing the correct traversal method based on the problem's requirements.

### Similar Questions

To further practice and deepen your understanding, consider working on the following similar questions:

1. **Binary Tree Level Order Traversal**
   - Given a binary tree, return the level order traversal of its nodes' values. (i.e., from left to right, level by level).
   - Example: Given binary tree `[3,9,20,null,null,15,7]`, return `[[3], [9, 20], [15, 7]]`.

2. **Binary Tree Zigzag Level Order Traversal**
   - Given a binary tree, return the zigzag level order traversal of its nodes' values. (i.e., from left to right, then right to left for the next level and alternate between).
   - Example: Given binary tree `[3,9,20,null,null,15,7]`, return `[[3], [20, 9], [15, 7]]`.

3. **Leftmost Nodes of a Binary Tree**
   - Given a binary tree, return the values of the nodes that you see when the tree is looked at from the left side.
   - Example: Given binary tree `[1,2,3,null,5,null,4]`, return `[1, 2, 5]`.

4. **Sum of Left Leaves**
   - Find the sum of all left leaves in a given binary tree.
   - Example: Given binary tree `[3,9,20,null,null,15,7]`, the sum is `24` (9 + 15).

5. **Maximum Depth of a Binary Tree**
   - Given a binary tree, find its maximum depth.
   - Example: Given binary tree `[3, 9, 20, null, null, 15, 7]`, return `3`.

Tackling these problems will not only enhance your skills in tree traversal techniques but also improve your problem-solving ability in various tree-related scenarios. Keep practicing and exploring different data structures to become proficient in solving complex coding challenges.