### Interviewer and Interviewee Discussion

**Interviewer:**
Let's discuss the problem of constructing a binary tree from given preorder and inorder traversal arrays. You are given two integer arrays, `preorder` and `inorder`, where `preorder` is the preorder traversal of a binary tree and `inorder` is the inorder traversal of the same tree. You need to construct the binary tree from these arrays and return its root.

**Interviewee:**
Got it. To clarify, in a preorder traversal, we visit the root node first, then recursively visit the left subtree, and then the right subtree. In an inorder traversal, we recursively visit the left subtree first, then the root node, and finally the right subtree.

**Interviewer:**
That's correct. Let's start by discussing a brute force approach to solve this problem. What are your initial thoughts?

### Initial Thoughts: Brute Force Approach

**Interviewee:**
In the brute force approach, we can follow these steps:
1. Use the first element of the `preorder` array as the root of the tree.
2. Find the index of this root in the `inorder` array.
3. Elements to the left of this index in the `inorder` array form the left subtree, and elements to the right form the right subtree.
4. Recursively apply the same process to construct the left and right subtrees.

**Interviewer:**
How do you plan to handle the splitting of the arrays for the recursive calls?

**Interviewee:**
We need to keep track of the current subtree boundaries in both `preorder` and `inorder` arrays. For each recursive call:
- The left subtree will have elements from the start of the current subtree to the found index (exclusive) in `inorder`.
- The right subtree will have elements after the found index to the end of the current subtree in `inorder`.

### Time and Space Complexity of Brute Force Approach

**Interviewee:**
The time complexity of this brute force approach is `O(n^2)` for the following reasons:
- Finding the root in the `inorder` array takes `O(n)` time.
- We do this for each node, leading to `O(n)` recursive calls.

The space complexity will be `O(n)` due to the recursion stack and the space needed to store the tree.

### Optimized Approach Using HashMap

**Interviewer:**
Can you think of a way to optimize this approach, especially to reduce the time complexity?

**Interviewee:**
We can use a HashMap to store the indices of each element in the `inorder` array. This will allow us to find the root index in `O(1)` time rather than `O(n)`.

### Plan

1. Create a HashMap to store the value-to-index mappings of the `inorder` array.
2. Use a helper function to construct the tree recursively with indices representing the current subtree boundaries.
3. Within the helper function:
   - Use the first element of the current `preorder` segment as the root.
   - Use the HashMap to find the root index in the `inorder` array.

### Implementation

Let's draw an example to explain this:

- Input: `preorder = [3, 9, 20, 15, 7]`, `inorder = [9, 3, 15, 20, 7]`

```
preorder:  3 | 9 | 20 | 15 | 7
inorder:     9 | 3 | 15 | 20 | 7
preIndex      ^
```

Here, `3` is the root. 

- Left subtree (inorder): `[9]`
- Right subtree (inorder): `[15, 20, 7]`

Now, repeat the process for subtrees:
- For left subtree `[9]`, `9` is the root with no children.
- For right subtree `[15, 20, 7]`, `20` is the root, with `15` as the left child and `7` as the right child.

### Code with Optimized Approach

```python
# Definition for a binary tree node.
class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

def buildTree(preorder, inorder):
    if not preorder or not inorder:
        return None

    # Step 1: Build a hashmap value -> its index for inorder traversal
    inorder_index_map = {val: idx for idx, val in enumerate(inorder)}

    # Step 2: Helper function to build the tree
    def buildSubTree(pre_left, pre_right, in_left, in_right):
        if pre_left > pre_right or in_left > in_right:
            return None
        
        root_val = preorder[pre_left]
        root = TreeNode(root_val)
        in_root_index = inorder_index_map[root_val]

        # Calculate the number of nodes in the left subtree
        left_subtree_size = in_root_index - in_left
        
        # Recursively build the left and right subtree
        root.left = buildSubTree(pre_left + 1, pre_left + left_subtree_size, in_left, in_root_index - 1)
        root.right = buildSubTree(pre_left + left_subtree_size + 1, pre_right, in_root_index + 1, in_right)
        
        return root

    return buildSubTree(0, len(preorder) - 1, 0, len(inorder) - 1)
```

### Optimized Time and Space Complexity

**Interviewee:**
The time complexity of this optimized approach is `O(n)` since we only traverse each node once and look up indices in `O(1)` using the HashMap.

The space complexity is `O(n)` due to the recursion stack and the space needed for the HashMap.
Sure, let's wrap the optimized algorithm into the methods provided for each language and include the time and space complexity explanations in comments.

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
#import <unordered_map>
class Solution {
public:
    TreeNode* buildTree(vector<int>& preorder, vector<int>& inorder) {
        std::unordered_map<int, int> inorder_index_map;
        for (int i = 0; i < inorder.size(); ++i) {
            inorder_index_map[inorder[i]] = i;
        }
        
        return buildSubTree(preorder, 0, preorder.size() - 1, inorder, 0, inorder.size() - 1, inorder_index_map);
    }
    
private:
    TreeNode* buildSubTree(vector<int>& preorder, int pre_left, int pre_right, 
                           vector<int>& inorder, int in_left, int in_right,
                           std::unordered_map<int, int>& inorder_index_map) {
        if (pre_left > pre_right || in_left > in_right) {
            return nullptr;
        }
        
        int root_val = preorder[pre_left];
        TreeNode* root = new TreeNode(root_val);
        int in_root_index = inorder_index_map[root_val];
        
        int left_subtree_size = in_root_index - in_left;
        
        root->left = buildSubTree(preorder, pre_left + 1, pre_left + left_subtree_size, 
                                  inorder, in_left, in_root_index - 1, inorder_index_map);
        root->right = buildSubTree(preorder, pre_left + left_subtree_size + 1, pre_right,
                                   inorder, in_root_index + 1, in_right, inorder_index_map);
        return root;
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
import java.util.HashMap;

class Solution {
    public TreeNode buildTree(int[] preorder, int[] inorder) {
        HashMap<Integer, Integer> inorderIndexMap = new HashMap<>();
        for (int i = 0; i < inorder.length; i++) {
            inorderIndexMap.put(inorder[i], i);
        }
        return buildSubTree(preorder, 0, preorder.length - 1,
                            inorder, 0, inorder.length - 1, 
                            inorderIndexMap);
    }
    
    private TreeNode buildSubTree(int[] preorder, int preLeft, int preRight,
                                  int[] inorder, int inLeft, int inRight,
                                  HashMap<Integer, Integer> inorderIndexMap) {
        if (preLeft > preRight || inLeft > inRight) {
            return null;
        }
        
        int rootVal = preorder[preLeft];
        TreeNode root = new TreeNode(rootVal);
        int inRootIndex = inorderIndexMap.get(rootVal);
        
        int leftSubtreeSize = inRootIndex - inLeft;
        root.left = buildSubTree(preorder, preLeft + 1, preLeft + leftSubtreeSize,
                                 inorder, inLeft, inRootIndex - 1,
                                 inorderIndexMap);
        root.right = buildSubTree(preorder, preLeft + leftSubtreeSize + 1, preRight,
                                  inorder, inRootIndex + 1, inRight,
                                  inorderIndexMap);
        return root;
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
    def buildTree(self, preorder, inorder):
        """
        :type preorder: List[int]
        :type inorder: List[int]
        :rtype: TreeNode
        """
        inorder_index_map = {val: idx for idx, val in enumerate(inorder)}
        
        def buildSubTree(pre_left, pre_right, in_left, in_right):
            if pre_left > pre_right or in_left > in_right:
                return None
            
            root_val = preorder[pre_left]
            root = TreeNode(root_val)
            in_root_index = inorder_index_map[root_val]
            
            left_subtree_size = in_root_index - in_left
            root.left = buildSubTree(pre_left + 1, pre_left + left_subtree_size, in_left, in_root_index - 1)
            root.right = buildSubTree(pre_left + left_subtree_size + 1, pre_right, in_root_index + 1, in_right)
            
            return root
        
        return buildSubTree(0, len(preorder) - 1, 0, len(inorder) - 1)

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
    def buildTree(self, preorder: List[int], inorder: List[int]) -> Optional[TreeNode]:
        inorder_index_map = {val: idx for idx, val in enumerate(inorder)}
        
        def buildSubTree(pre_left, pre_right, in_left, in_right):
            if pre_left > pre_right or in_left > in_right:
                return None
            
            root_val = preorder[pre_left]
            root = TreeNode(root_val)
            in_root_index = inorder_index_map[root_val]
            
            left_subtree_size = in_root_index - in_left
            root.left = buildSubTree(pre_left + 1, pre_left + left_subtree_size, in_left, in_root_index - 1)
            root.right = buildSubTree(pre_left + left_subtree_size + 1, pre_right, in_root_index + 1, in_right)
            
            return root
        
        return buildSubTree(0, len(preorder) - 1, 0, len(inorder) - 1)

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
#include <stdlib.h>

struct TreeNode {
    int val;
    struct TreeNode *left;
    struct TreeNode *right;
};

int* buildMap(int* inorder, int inorderSize) {
    int* map = (int*)malloc(sizeof(int) * 6001);
    for (int i = 0; i < inorderSize; i++) {
        map[inorder[i] + 3000] = i;
    }
    return map;
}

struct TreeNode* buildSubTree(int* preorder, int pre_left, int pre_right,
                              int* inorder, int in_left, int in_right,
                              int* inorderIndexMap) {
    if (pre_left > pre_right || in_left > in_right) {
        return NULL;
    }
    
    int root_val = preorder[pre_left];
    struct TreeNode* root = (struct TreeNode*)malloc(sizeof(struct TreeNode));
    root->val = root_val;
    root->left = root->right = NULL;
    
    int in_root_index = inorderIndexMap[root_val + 3000];
    int left_subtree_size = in_root_index - in_left;
    
    root->left = buildSubTree(preorder, pre_left + 1, pre_left + left_subtree_size, 
                              inorder, in_left, in_root_index - 1,
                              inorderIndexMap);
    root->right = buildSubTree(preorder, pre_left + left_subtree_size + 1, pre_right, 
                               inorder, in_root_index + 1, in_right,
                               inorderIndexMap);
    return root;
}

struct TreeNode* buildTree(int* preorder, int preorderSize, int* inorder, int inorderSize) {
    int* inorderIndexMap = buildMap(inorder, inorderSize);
    struct TreeNode* result = buildSubTree(preorder, 0, preorderSize - 1, 
                                           inorder, 0, inorderSize - 1,
                                           inorderIndexMap);
    free(inorderIndexMap);
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
    public TreeNode BuildTree(int[] preorder, int[] inorder) {
        var inorderIndexMap = new Dictionary<int, int>();
        for (int i = 0; i < inorder.Length; i++) {
            inorderIndexMap[inorder[i]] = i;
        }
        
        return BuildSubTree(preorder, 0, preorder.Length - 1,
                            inorder, 0, inorder.Length - 1,
                            inorderIndexMap);
    }
    
    private TreeNode BuildSubTree(int[] preorder, int preLeft, int preRight,
                                  int[] inorder, int inLeft, int inRight,
                                  Dictionary<int, int> inorderIndexMap) {
        if (preLeft > preRight || inLeft > inRight) {
            return null;
        }
        
        int rootVal = preorder[preLeft];
        TreeNode root = new TreeNode(rootVal);
        int inRootIndex = inorderIndexMap[rootVal];
        
        int leftSubtreeSize = inRootIndex - inLeft;
        root.left = BuildSubTree(preorder, preLeft + 1, preLeft + leftSubtreeSize,
                                 inorder, inLeft, inRootIndex - 1,
                                 inorderIndexMap);
        root.right = BuildSubTree(preorder, preLeft + leftSubtreeSize + 1, preRight,
                                  inorder, inRootIndex + 1, inRight,
                                  inorderIndexMap);
        return root;
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
 * @param {number[]} preorder
 * @param {number[]} inorder
 * @return {TreeNode}
 */
var buildTree = function(preorder, inorder) {
    const inorderIndexMap = new Map();
    inorder.forEach((val, idx) => inorderIndexMap.set(val, idx));

    const buildSubTree = (preLeft, preRight, inLeft, inRight) => {
        if (preLeft > preRight || inLeft > inRight) return null;
        
        const rootVal = preorder[preLeft];
        const root = new TreeNode(rootVal);
        const inRootIndex = inorderIndexMap.get(rootVal);
        
        const leftSubtreeSize = inRootIndex - inLeft;
        root.left = buildSubTree(preLeft + 1, preLeft + leftSubtreeSize, inLeft, inRootIndex - 1);
        root.right = buildSubTree(preLeft + leftSubtreeSize + 1, preRight, inRootIndex + 1, inRight);
        
        return root;
    };

    return buildSubTree(0, preorder.length - 1, 0, inorder.length - 1);
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

function buildTree(preorder: number[], inorder: number[]): TreeNode | null {
    const inorderIndexMap = new Map<number, number>();
    inorder.forEach((val, idx) => inorderIndexMap.set(val, idx));
    
    const buildSubTree = (preLeft: number, preRight: number, inLeft: number, inRight: number): TreeNode | null => {
        if (preLeft > preRight || inLeft > inRight) return null;
        
        const rootVal = preorder[preLeft];
        const root = new TreeNode(rootVal);
        const inRootIndex = inorderIndexMap.get(rootVal)!;
        
        const leftSubtreeSize = inRootIndex - inLeft;
        root.left = buildSubTree(preLeft + 1, preLeft + leftSubtreeSize, inLeft, inRootIndex - 1);
        root.right = buildSubTree(preLeft + leftSubtreeSize + 1, preRight, inRootIndex + 1, inRight);
        
        return root;
    };
    
    return buildSubTree(0, preorder.length - 1, 0, inorder.length - 1);
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
     * @param Integer[] $preorder
     * @param Integer[] $inorder
     * @return TreeNode
     */
    function buildTree($preorder, $inorder) {
        $inorderIndexMap = [];
        foreach ($inorder as $key => $value) {
            $inorderIndexMap[$value] = $key;
        }

        return $this->buildSubTree($preorder, 0, count($preorder) - 1, $inorder, 0, count($inorder) - 1, $inorderIndexMap);
    }

    function buildSubTree($preorder, $preLeft, $preRight, $inorder, $inLeft, $inRight, $inorderIndexMap) {
        if ($preLeft > $preRight || $inLeft > $inRight) return null;

        $rootVal = $preorder[$preLeft];
        $root = new TreeNode($rootVal);
        $inRootIndex = $inorderIndexMap[$rootVal];

        $leftSubtreeSize = $inRootIndex - $inLeft;
        $root->left = $this->buildSubTree($preorder, $preLeft + 1, $preLeft + $leftSubtreeSize, $inorder, $inLeft, $inRootIndex - 1, $inorderIndexMap);
        $root->right = $this->buildSubTree($preorder, $preLeft + $leftSubtreeSize + 1, $preRight, $inorder, $inRootIndex + 1, $inRight, $inorderIndexMap);

        return $root;
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
    func buildTree(_ preorder: [Int], _ inorder: [Int]) -> TreeNode? {
        var inorderIndexMap = [Int: Int]()
        for (index, value) in inorder.enumerated() {
            inorderIndexMap[value] = index
        }
        
        return buildSubTree(preorder, 0, preorder.count - 1, 
                            inorder, 0, inorder.count - 1,
                            inorderIndexMap)
    }
    
    private func buildSubTree(_ preorder: [Int], _ preLeft: Int, _ preRight: Int,
                              _ inorder: [Int], _ inLeft: Int, _ inRight: Int,
                              _ inorderIndexMap: [Int: Int]) -> TreeNode? {
        if preLeft > preRight || inLeft > inRight {
            return nil
        }
        
        let rootVal = preorder[preLeft]
        let root = TreeNode(rootVal)
        let inRootIndex = inorderIndexMap[rootVal]!
        
        let leftSubtreeSize = inRootIndex - inLeft
        root.left = buildSubTree(preorder, preLeft + 1, preLeft + leftSubtreeSize,
                                 inorder, inLeft, inRootIndex - 1,
                                 inorderIndexMap)
        root.right = buildSubTree(preorder, preLeft + leftSubtreeSize + 1, preRight,
                                  inorder, inRootIndex + 1, inRight,
                                  inorderIndexMap)
        return root
    }
}

// Time Complexity: O(n)
// Space Complexity: O(n)
```


### Closing Statement

**Interviewer:**
Great job! You’ve successfully demonstrated your ability to construct a binary tree from given preorder and inorder traversals using a hash map for optimization. You've explained your thought process clearly and efficiently, taking into account both the brute force and optimized approaches with detailed complexity analysis. Well done!

**Interviewee:**
Thank you! I'm glad I could walk through the problem and provide an optimized solution. This was a very insightful exercise in tree manipulations and efficient data structures.

### Similar Questions

Here are a few questions that involve similar concepts and might interest you:

1. **Construct Binary Tree from Inorder and Postorder Traversal:**
   - Given the inorder and postorder traversal of a binary tree, construct and return the binary tree.
2. **Construct Binary Search Tree from Preorder Traversal:**
   - Construct a Binary Search Tree from a preorder traversal array.
3. **Verify Preorder Serialization of a Binary Tree:**
   - Given a string of comma-separated values, find if it is a valid preorder serialization of a binary tree.
4. **Find the Postorder Traversal of a Binary Tree:**
   - Given the inorder and preorder traversal, find the postorder traversal of a binary tree without constructing the tree.
5. **Convert Sorted Array to Binary Search Tree:**
   - Given an array where elements are sorted in ascending order, convert it to a height-balanced Binary Search Tree.

These questions will help you reinforce your understanding of tree construction and traversal. Good luck with your preparation!