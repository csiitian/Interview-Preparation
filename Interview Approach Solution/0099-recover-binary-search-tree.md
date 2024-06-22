### Interviewer and Interviewee Discussion

**Interviewer:** Let's discuss the problem where you need to recover a Binary Search Tree (BST) in which exactly two nodes were swapped by mistake. You need to rectify the tree without altering its structure. How would you approach solving this?

**Interviewee:** First, let's talk about the properties of a BST. In a BST, for any given node, all nodes in its left subtree are smaller and all nodes in its right subtree are larger. If two nodes are swapped, this property is violated. 

To detect and correct the swapped nodes, we can do an inorder traversal of the tree. An inorder traversal of a BST normally gives nodes in sorted order. If we identify where the order is violated, we can identify the swapped nodes.

**Interviewer:** That makes sense. Let's discuss a brute-force approach first. How would you implement that?

**Interviewee:** Sure, for the brute-force approach:

1. Perform an inorder traversal of the tree.
2. Store the traversal result in a list.
3. Identify the two elements in the list that are out of order.
4. Swap these two elements.
5. Finally, reconstruct the tree using the corrected list.

This approach will take \(O(n)\) space to store the inorder traversal and \(O(n)\) time for traversal and reconstruction.

### Brute-force Approach
```python
def inorder_traversal(root, output):
    if not root:
        return
    inorder_traversal(root.left, output)
    output.append(root)
    inorder_traversal(root.right, output)

def recover_tree(root):
    inorder_nodes = []
    inorder_traversal(root, inorder_nodes)

    first = second = None
    for i in range(len(inorder_nodes) - 1):
        if inorder_nodes[i].val > inorder_nodes[i + 1].val:
            if not first:
                first = inorder_nodes[i]
            second = inorder_nodes[i + 1]
    
    # Swap the values of the first and second nodes
    first.val, second.val = second.val, first.val
```
**Interviewer:** What about the time and space complexity of this brute-force approach?

**Interviewee:** 
- **Time Complexity:** \(O(n)\) because we perform an inorder traversal of the tree.
- **Space Complexity:** \(O(n)\) because we store all the nodes in the list.

**Interviewer:** Can you think of a way to optimize this solution further, particularly concerning space complexity?

**Interviewee:** Yes, we can optimize space usage by performing a Morris Traversal which allows us to perform an inorder traversal with \(O(1)\) space complexity by temporarily modifying the tree structure.

### Optimized Approach using Morris Traversal

Let's draw our approach:

1. Traverse the tree using Morris Traversal while looking for misplaced nodes.
2. While traversing, find the two nodes whose values are out of order.
3. Swap the values of these nodes.
4. Restore the tree if any temporary modifications were made.

**Diagram:**

Initially, let's consider a scenario where the nodes 1 and 3 are swapped in a BST:

```
   3
  / \
 1   4
     /
    2
```

Steps in Morris Traversal:
- Traverse node 3, move to left child node 1.
- Node 1 has no left child, visit node 1. Move right to node 3, which has left subtree restored.
- Visit node 3, move right to node 4.
- Visit node 4, move left to node 2.
- Node 2 has no left child, visit node 2. Move right to node 4, which has left subtree restored.

In our case, we found that nodes 3 and 1 were in the wrong order. Swap their values to get the correct BST:

```
   1
  / \
 3   4
     /
    2
```

Let's implement this in code:

```python
class Solution:
    def recoverTree(self, root):
        def morrisTraversal(root):
            current = root
            first = second = prev = None

            while current:
                if not current.left:
                    if prev and current.val < prev.val:
                        second = current
                        if not first:
                            first = prev
                    prev = current
                    current = current.right
                else:
                    pre = current.left
                    while pre.right and pre.right is not current:
                        pre = pre.right

                    if not pre.right:
                        pre.right = current
                        current = current.left
                    else:
                        pre.right = None
                        if prev and current.val < prev.val:
                            second = current
                            if not first:
                                first = prev
                        prev = current
                        current = current.right
            return first, second

        first, second = morrisTraversal(root)
        first.val, second.val = second.val, first.val
```

**Interviewer:** Great! Can you discuss the time and space complexity for this optimized approach?

**Interviewee:** 
- **Time Complexity:** \(O(n)\), since we still traverse all nodes once.
- **Space Complexity:** \(O(1)\), as we avoid using extra space for the inorder list and use Morris Traversal to traverse the tree in-place.

**Interviewer:** Excellent! This looks efficient and well-thought-out. Thank you for your explanation.
Sure! Let's go through the implementation for the `recoverTree` function in each of the provided languages, incorporating the optimized Morris Traversal approach.

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
    void recoverTree(TreeNode* root) {
        TreeNode *first = nullptr, *second = nullptr, *prev = nullptr, *pred = nullptr;

        while (root) {
            if (!root->left) {
                if (prev && root->val < prev->val) {
                    second = root;
                    if (!first) first = prev;
                }
                prev = root;
                root = root->right;
            } else {
                pred = root->left;
                while (pred->right && pred->right != root) pred = pred->right;

                if (!pred->right) {
                    pred->right = root;
                    root = root->left;
                } else {
                    pred->right = nullptr;
                    if (prev && root->val < prev->val) {
                        second = root;
                        if (!first) first = prev;
                    }
                    prev = root;
                    root = root->right;
                }
            }
        }
        swap(first->val, second->val);
    }
};
```

### Java
```java
/**
 * Definition for a binary tree node.
 * public class TreeNode {
 *     int val;
 *     TreeNode left, right;
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
    public void recoverTree(TreeNode root) {
        TreeNode first = null, second = null, prev = null, pred = null;

        TreeNode curr = root;
        while (curr != null) {
            if (curr.left == null) {
                if (prev != null && curr.val < prev.val) {
                    second = curr;
                    if (first == null) first = prev;
                }
                prev = curr;
                curr = curr.right;
            } else {
                pred = curr.left;
                while (pred.right != null && pred.right != curr) {
                    pred = pred.right;
                }
                if (pred.right == null) {
                    pred.right = curr;
                    curr = curr.left;
                } else {
                    pred.right = null;
                    if (prev != null && curr.val < prev.val) {
                        second = curr;
                        if (first == null) first = prev;
                    }
                    prev = curr;
                    curr = curr.right;
                }
            }
        }
        if (first != null && second != null) {
            int temp = first.val;
            first.val = second.val;
            second.val = temp;
        }
    }
}
```

### Python
```python
class Solution(object):
    def recoverTree(self, root):
        """
        :type root: TreeNode
        :rtype: None
        """
        first = second = prev = pred = None
        current = root

        while current:
            if not current.left:
                if prev and current.val < prev.val:
                    second = current
                    if not first:
                        first = prev
                prev = current
                current = current.right
            else:
                pred = current.left
                while pred.right and pred.right is not current:
                    pred = pred.right
                if not pred.right:
                    pred.right = current
                    current = current.left
                else:
                    pred.right = None
                    if prev and current.val < prev.val:
                        second = current
                        if not first:
                            first = prev
                    prev = current
                    current = current.right

        first.val, second.val = second.val, first.val
```

### Python3
```python
class Solution:
    def recoverTree(self, root: Optional[TreeNode]) -> None:
        """
        Do not return anything, modify root in-place instead.
        """
        first = second = prev = pred = None
        current = root

        while current:
            if not current.left:
                if prev and current.val < prev.val:
                    second = current
                    if not first:
                        first = prev
                prev = current
                current = current.right
            else:
                pred = current.left
                while pred.right and pred.right is not current:
                    pred = pred.right
                if not pred.right:
                    pred.right = current
                    current = current.left
                else:
                    pred.right = None
                    if prev and current.val < prev.val:
                        second = current
                        if not first:
                            first = prev
                    prev = current
                    current = current.right

        first.val, second.val = second.val, first.val
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
void recoverTree(struct TreeNode* root) {
    struct TreeNode *first = NULL, *second = NULL, *prev = NULL, *pred = NULL;
    struct TreeNode *current = root;

    while (current) {
        if (!current->left) {
            if (prev && current->val < prev->val) {
                second = current;
                if (!first) first = prev;
            }
            prev = current;
            current = current->right;
        } else {
            pred = current->left;
            while (pred->right && pred->right != current) pred = pred->right;
            
            if (!pred->right) {
                pred->right = current;
                current = current->left;
            } else {
                pred->right = NULL;
                if (prev && current->val < prev->val) {
                    second = current;
                    if (!first) first = prev;
                }
                prev = current;
                current = current->right;
            }
        }
    }
    int temp = first->val;
    first->val = second->val;
    second->val = temp;
}
```


### Closing Statement

Great job discussing and implementing the optimal approach to recover a Binary Search Tree where exactly two nodes have been swapped. By leveraging Morris Traversal, you were able to achieve an in-place solution with `O(n)` time complexity and `O(1)` space complexity. This approach ensures that we detect and correct the misplaced nodes without the need for additional space, making our solution both efficient and elegant. 

You've demonstrated a strong understanding of tree traversal techniques and their applications in solving complex problems. Keep practicing such optimization skills to further enhance your problem-solving abilities.

### Similar Questions for Practice

1. **Validate Binary Search Tree**: 
   - **Description**: Given a binary tree, determine if it is a valid binary search tree (BST).
   - **Link**: [Validate Binary Search Tree on Leetcode](https://leetcode.com/problems/validate-binary-search-tree/)

2. **Binary Tree Inorder Traversal**:
   - **Description**: Given a binary tree, return the inorder traversal of its nodes' values.
   - **Link**: [Binary Tree Inorder Traversal on Leetcode](https://leetcode.com/problems/binary-tree-inorder-traversal/)

3. **Convert Sorted Array to Binary Search Tree**:
   - **Description**: Given an array where elements are sorted in ascending order, convert it to a height-balanced binary search tree.
   - **Link**: [Convert Sorted Array to Binary Search Tree on Leetcode](https://leetcode.com/problems/convert-sorted-array-to-binary-search-tree/)

4. **Kth Smallest Element in a BST**:
   - **Description**: Given a binary search tree, write a function to find the kth smallest element in it.
   - **Link**: [Kth Smallest Element in a BST on Leetcode](https://leetcode.com/problems/kth-smallest-element-in-a-bst/)

5. **Find Mode in Binary Search Tree**:
   - **Description**: Given a binary search tree (BST) with duplicates, find all the mode(s) (the most frequently occurred element) in the given BST.
   - **Link**: [Find Mode in Binary Search Tree on Leetcode](https://leetcode.com/problems/find-mode-in-binary-search-tree/)

These questions will help you further solidify your understanding and provide more opportunities to hone your skills in tree-related problems. Happy Coding!