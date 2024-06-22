### Interviewer and Interviewee Discussion

**Interviewer:**
We have a problem here. Given the head of a singly linked list where the elements are sorted in ascending order, convert it into a height-balanced binary search tree (BST). Let's go through an example first. If the linked list is `[-10, -3, 0, 5, 9]`, one possible output for a height-balanced BST would be `[0, -3, 9, -10, null, 5]`. How would you approach this problem?

**Interviewee:**
To solve this problem, we need to understand two key concepts:
1. The linked list is sorted in ascending order.
2. We need to form a height-balanced BST, which means for each node, the depth of the left subtree and the right subtree should not differ by more than one.

**Interviewer:**
Good. What’s the initial approach you would consider?

**Interviewee:**
A brute force approach would involve the following steps:
1. Convert the linked list to an array first. This lets us access elements in constant time.
2. Use a recursive approach to build the tree from this array.
   - Find the middle element of the array to make it the root of the BST.
   - Recursively do the same for the left half to form the left subtree and the right half to form the right subtree.

**Interviewer:**
That makes sense. Explain the brute force approach in more detail with the time and space complexity.

### Brute Force Approach

1. **Convert Linked List to Array:**
   - Traverse the linked list to copy all values into an array.
   
2. **Build BST from Array:**
   - Use the middle element of the array as the root.
   - Recursively do the same for the left and right subarrays to create left and right subtrees respectively.

**Time Complexity:**
- Converting the linked list to an array: \(O(n)\), where \(n\) is the number of nodes in the linked list.
- Building the BST from the array: \(O(n)\) because each recursive step processes every element exactly once.

Overall time complexity is \(O(n)\).

**Space Complexity:**
- Additional space for the array: \(O(n)\).
- Recursion stack space: \(O(\log n)\) for a balanced tree.

Overall space complexity is \(O(n)\).

```python
def sorted_list_to_bst(head):
    # Step 1: Convert linked list to array
    values = []
    while head:
        values.append(head.val)
        head = head.next
    
    # Step 2: Helper function to build the BST from array
    def build_bst(vals):
        if not vals:
            return None
        mid = len(vals) // 2
        root = TreeNode(vals[mid])
        root.left = build_bst(vals[:mid])
        root.right = build_bst(vals[mid+1:])
        return root
    
    return build_bst(values)
```

**Interviewer:**
Great! Given we have the brute force approach, is there a more efficient way to build the BST without using extra space for the array?

**Interviewee:**
Yes, we can enhance the approach to avoid using extra space by performing an in-order traversal of the linked list while creating the BST. This way, we can construct the tree directly from the linked list.

### Optimized Approach Using In-Order Traversal

**Idea:**
1. Use a slow and fast pointer approach to find the middle element of the linked list to use as the root.
2. Recursively convert the left part of the linked list to the left subtree and the right part to the right subtree.

This discards the need for an additional array, thus reducing space complexity.

**Steps:**
1. Find the middle of the linked list (this will be the root).
2. Recursively split the list into two halves and repeat the process for each half.

**Time Complexity:**
- Each instance of finding the middle does a traversal, so it's \(O(n)\) for each recursive call, but there are \(log n\) levels of recursion.

So, the time complexity remains \(O(n)\).

**Space Complexity:**
- We only use space for the recursion stack which is \(O(\log n)\).

```python
class TreeNode:
    def __init__(self, val=0, left=None, right=None):
        self.val = val
        self.left = left
        self.right = right

def find_middle_and_split(head):
    prev_ptr = None
    slow_ptr = head
    fast_ptr = head
    
    while fast_ptr and fast_ptr.next:
        prev_ptr = slow_ptr
        slow_ptr = slow_ptr.next
        fast_ptr = fast_ptr.next.next
    
    if prev_ptr:
        prev_ptr.next = None

    return slow_ptr

def sorted_list_to_bst(head):
    if not head:
        return None

    mid = find_middle_and_split(head)
    
    node = TreeNode(mid.val)
    
    if head == mid:
        return node
    
    node.left = sorted_list_to_bst(head)
    node.right = sorted_list_to_bst(mid.next)
    
    return node
```

**Diagram:**
Let's visualize this with the example `[-10, -3, 0, 5, 9]`.

1. Initial List: `[-10, -3, 0, 5, 9]`
2. Root is `0` (middle element)
3. Left subtree from `[-10, -3]`
    - Root is `-3`
    - Left subtree from `[-10]` (Leaf node `-10`)
4. Right subtree from `[5, 9]`
    - Root is `9`
    - Left subtree from `[5]` (Leaf node `5`)

Thus, the constructed BST would be:

```
         0
       /   \
     -3     9
     /     /
   -10    5
```

Perfect! This approach optimizes space while maintaining efficiency.
Certainly! Here are the implementations for the optimized solution in each of the requested languages.

### C++

```cpp
/**
 * Definition for singly-linked list.
 * struct ListNode {
 *     int val;
 *     ListNode *next;
 *     ListNode() : val(0), next(nullptr) {}
 *     ListNode(int x) : val(x), next(nullptr) {}
 *     ListNode(int x, ListNode *next) : val(x), next(next) {}
 * };
 */

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
    TreeNode* sortedListToBST(ListNode* head) {
        if (!head) return nullptr;
        
        ListNode* mid = findMiddleAndSplit(head);
        
        TreeNode* node = new TreeNode(mid->val);
        
        if (head == mid) {
            return node;
        }
        
        node.left = sortedListToBST(head);
        node.right = sortedListToBST(mid->next);
        
        return node;
    }

private:
    ListNode* findMiddleAndSplit(ListNode* head) {
        ListNode* prev_ptr = nullptr;
        ListNode* slow_ptr = head;
        ListNode* fast_ptr = head;
        
        while (fast_ptr != nullptr && fast_ptr->next != nullptr) {
            prev_ptr = slow_ptr;
            slow_ptr = slow_ptr->next;
            fast_ptr = fast_ptr->next->next;
        }
        
        if (prev_ptr != nullptr) {
            prev_ptr->next = nullptr;
        }
        
        return slow_ptr;
    }
};
```

### Java

```java
/**
 * Definition for singly-linked list.
 * public class ListNode {
 *     int val;
 *     ListNode next;
 *     ListNode() {}
 *     ListNode(int val) { this.val = val; }
 *     ListNode(int val, ListNode next) { this.val = val; this.next = next; }
 * }
 */

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
    public TreeNode sortedListToBST(ListNode head) {
        if (head == null) return null;
        
        ListNode mid = findMiddleAndSplit(head);
        
        TreeNode node = new TreeNode(mid.val);
        
        if (head == mid) {
            return node;
        }
        
        node.left = sortedListToBST(head);
        node.right = sortedListToBST(mid.next);
        
        return node;
    }

    private ListNode findMiddleAndSplit(ListNode head) {
        ListNode prevPtr = null;
        ListNode slowPtr = head;
        ListNode fastPtr = head;
        
        while (fastPtr != null && fastPtr.next != null) {
            prevPtr = slowPtr;
            slowPtr = slowPtr.next;
            fastPtr = fastPtr.next.next;
        }
        
        if (prevPtr != null) {
            prevPtr.next = null;
        }
        
        return slowPtr;
    }
}
```

### Python

```python
# Definition for singly-linked list.
# class ListNode(object):
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next

# Definition for a binary tree node.
# class TreeNode(object):
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right

class Solution(object):
    def sortedListToBST(self, head):
        """
        :type head: Optional[ListNode]
        :rtype: Optional[TreeNode]
        """
        def find_middle_and_split(head):
            prev_ptr = None
            slow_ptr = head
            fast_ptr = head
            
            while fast_ptr and fast_ptr.next:
                prev_ptr = slow_ptr
                slow_ptr = slow_ptr.next
                fast_ptr = fast_ptr.next.next
            
            if prev_ptr:
                prev_ptr.next = None
            
            return slow_ptr
        
        if not head:
            return None
        
        mid = find_middle_and_split(head)
        
        node = TreeNode(mid.val)
        
        if head == mid:
            return node
        
        node.left = self.sortedListToBST(head)
        node.right = self.sortedListToBST(mid.next)
        
        return node
```

### Python3

```python
# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next

# Definition for a binary tree node.
# class TreeNode:
#     def __init__(self, val=0, left=None, right=None):
#         self.val = val
#         self.left = left
#         self.right = right

class Solution:
    def sortedListToBST(self, head: Optional[ListNode]) -> Optional[TreeNode]:
        def find_middle_and_split(head):
            prev_ptr = None
            slow_ptr = head
            fast_ptr = head
            
            while fast_ptr and fast_ptr.next:
                prev_ptr = slow_ptr
                slow_ptr = slow_ptr.next
                fast_ptr = fast_ptr.next.next
            
            if prev_ptr:
                prev_ptr.next = None
            
            return slow_ptr
        
        if not head:
            return None
        
        mid = find_middle_and_split(head)
        
        node = TreeNode(mid.val)
        
        if head == mid:
            return node
        
        node.left = self.sortedListToBST(head)
        node.right = self.sortedListToBST(mid.next)
        
        return node
```

### C

```c
/**
 * Definition for singly-linked list.
 * struct ListNode {
 *     int val;
 *     struct ListNode *next;
 * };
 */

/**
 * Definition for a binary tree node.
 * struct TreeNode {
 *     int val;
 *     struct TreeNode *left;
 *     struct TreeNode *right;
 * };
 */

struct ListNode* find_middle_and_split(struct ListNode* head) {
    struct ListNode* prev_ptr = NULL;
    struct ListNode* slow_ptr = head;
    struct ListNode* fast_ptr = head;
    
    while (fast_ptr != NULL && fast_ptr->next != NULL) {
        prev_ptr = slow_ptr;
        slow_ptr = slow_ptr->next;
        fast_ptr = fast_ptr->next->next;
    }
    
    if (prev_ptr != NULL) {
        prev_ptr->next = NULL;
    }
    
    return slow_ptr;
}

struct TreeNode* newTreeNode(int val) {
    struct TreeNode* node = (struct TreeNode*)malloc(sizeof(struct TreeNode));
    node->val = val;
    node->left = NULL;
    node->right = NULL;
    return node;
}

struct TreeNode* sortedListToBST(struct ListNode* head) {
    if (head == NULL) return NULL;
    
    struct ListNode* mid = find_middle_and_split(head);
    
    struct TreeNode* node = newTreeNode(mid->val);
    
    if (head == mid) {
        return node;
    }
    
    node->left = sortedListToBST(head);
    node->right = sortedListToBST(mid->next);
    
    return node;
}
```

### C#

```csharp
/**
 * Definition for singly-linked list.
 * public class ListNode {
 *     public int val;
 *     public ListNode next;
 *     public ListNode(int val=0, ListNode next=null) {
 *         this.val = val;
 *         this.next = next;
 *     }
 * }
 */

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
    public TreeNode SortedListToBST(ListNode head) {
        if (head == null) return null;
        
        ListNode mid = FindMiddleAndSplit(head);
        
        TreeNode node = new TreeNode(mid.val);
        
        if (head == mid) {
            return node;
        }
        
        node.left = SortedListToBST(head);
        node.right = SortedListToBST(mid.next);
        
        return node;
    }

    private ListNode FindMiddleAndSplit(ListNode head) {
        ListNode prevPtr = null;
        ListNode slowPtr = head;
        ListNode fastPtr = head;
        
        while (fastPtr != null && fastPtr.next != null) {
            prevPtr = slowPtr;
            slowPtr = slowPtr.next;
            fastPtr = fastPtr.next.next;
        }
        
        if (prevPtr != null) {
            prevPtr.next = null;
        }
        
        return slowPtr;
    }
}
```

### JavaScript

```javascript
/**
 * Definition for singly-linked list.
 * function ListNode(val, next) {
 *     this.val = (val===undefined ? 0 : val)
 *     this.next = (next===undefined ? null : next)
 * }
 */

/**
 * Definition for a binary tree node.
 * function TreeNode(val, left, right) {
 *     this.val = (val===undefined ? 0 : val)
 *     this.left = (left===undefined ? null : left)
 *     this.right = (right===undefined ? null : right)
 * }
 */

/**
 * @param {ListNode} head
 * @return {TreeNode}
 */
var sortedListToBST = function(head) {
    function findMiddleAndSplit(head) {
        let prevPtr = null;
        let slowPtr = head;
        let fastPtr = head;
        
        while (fastPtr && fastPtr.next) {
            prevPtr = slowPtr;
            slowPtr = slowPtr.next;
            fastPtr = fastPtr.next.next;
        }
        
        if (prevPtr) {
            prevPtr.next = null;
        }
        
        return slowPtr;
    }
    
    if (!head) return null;
    
    const mid = findMiddleAndSplit(head);
    
    const node = new TreeNode(mid.val);
    
    if (head === mid) {
        return node;
    }
    
    node.left = sortedListToBST(head);
    node.right = sortedListToBST(mid.next);
    
    return node;
};
```

### TypeScript

```typescript
/**
 * Definition for singly-linked list.
 * class ListNode {
 *     val: number
 *     next: ListNode | null
 *     constructor(val?: number, next?: ListNode | null) {
 *         this.val = (val===undefined ? 0 : val)
 *         this.next = (next===undefined ? null : next)
 *     }
 * }
 */

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

function sortedListToBST(head: ListNode | null): TreeNode | null {
    function findMiddleAndSplit(head: ListNode | null): ListNode | null {
        let prevPtr: ListNode | null = null;
        let slowPtr: ListNode | null = head;
        let fastPtr: ListNode | null = head;
        
        while (fastPtr != null && fastPtr.next != null) {
            prevPtr = slowPtr;
            slowPtr = slowPtr.next;
            fastPtr = fastPtr.next.next;
        }
        
        if (prevPtr != null) {
            prevPtr.next = null;
        }
        
        return slowPtr;
    }
    
    if (head === null) return null;
    
    const mid = findMiddleAndSplit(head);
    
    const node = new TreeNode(mid.val);
    
    if (head === mid) {
        return node;
    }
    
    node.left = sortedListToBST(head);
    node.right = sortedListToBST(mid.next);
    
    return node;
};
```

### PHP

```php
/**
 * Definition for a singly-linked list.
 * class ListNode {
 *     public $val = 0;
 *     public $next = null;
 *     function __construct($val = 0, $next = null) {
 *         $this->val = $val;
 *         $this->next = $next;
 *     }
 * }
 */

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
     * @param ListNode $head
     * @return TreeNode
     */
    function sortedListToBST($head) {
        if ($head === null) return null;
        
        $mid = $this->findMiddleAndSplit($head);
        
        $node = new TreeNode($mid->val);
        
        if ($head === $mid) {
            return $node;
        }
        
        $node->left = $this->sortedListToBST($head);
        $node->right = $this->sortedListToBST($mid->next);
        
        return $node;
    }

    function findMiddleAndSplit($head) {
        $prevPtr = null;
        $slowPtr = $head;
        $fastPtr = $head;
        
        while ($fastPtr !== null && $fastPtr->next !== null) {
            $prevPtr = $slowPtr;
            $slowPtr = $slowPtr->next;
            $fastPtr = $fastPtr->next->next;
        }
        
        if ($prevPtr !== null) {
            $prevPtr->next = null;
        }
        
        return $slowPtr;
    }
}
```

### Swift

```swift
/**
 * Definition for singly-linked list.
 * public class ListNode {
 *     public var val: Int
 *     public var next: ListNode?
 *     public init() { self.val = 0; self.next = nil; }
 *     public init(_ val: Int) { self.val = val; self.next = nil; }
 *     public init(_ val: Int, _ next: ListNode?) { self.val = val; self.next = next; }
 * }
 */

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
    func sortedListToBST(_ head: ListNode?) -> TreeNode? {
        func findMiddleAndSplit(_ head: ListNode?) -> ListNode? {
            var prevPtr: ListNode? = nil
            var slowPtr: ListNode? = head
            var fastPtr: ListNode? = head
            
            while fastPtr != nil && fastPtr?.next != nil {
                prevPtr = slowPtr
                slowPtr = slowPtr?.next
                fastPtr = fastPtr?.next?.next
            }
            
            if prevPtr != nil {
                prevPtr?.next = nil
            }
            
            return slowPtr
        }
        
        if head == nil {
            return nil
        }
        
        let mid = findMiddleAndSplit(head)
        
        let node = TreeNode(mid!.val)
        
        if head === mid {
            return node
        }
        
        node.left = sortedListToBST(head)
        node.right = sortedListToBST(mid?.next)
        
        return node
    }
}
```

### Kotlin

```kotlin
/**
 * Example:
 * var li = ListNode(5)
 * var v = li.`val`
 * Definition for singly-linked list.
 * class ListNode(var `val`: Int) {
 *     var next: ListNode? = null
 * }
 */

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
    fun sortedListToBST(head: ListNode?): TreeNode? {
        fun findMiddleAndSplit(head: ListNode?): ListNode? {
            var prevPtr: ListNode? = null
            var slowPtr: ListNode? = head
            var fastPtr: ListNode? = head
            
            while (fastPtr != null && fastPtr.next != null) {
                prevPtr = slowPtr
                slowPtr = slowPtr?.next
                fastPtr = fastPtr.next?.next
            }
            
            if (prevPtr != null) {
                prevPtr.next = null
            }
            
            return slowPtr
        }
        
        if (head == null) return null
        
        val mid = findMiddleAndSplit(head)
        
        val node = TreeNode(mid!!.`val`)
        
        if (head == mid) {
            return node
        }
        
        node.left = sortedListToBST(head)
        node.right = sortedListToBST(mid.next)
        
        return node
    }
}
```


### Closing Statement

In our discussion, we tackled the problem of converting a sorted singly linked list into a height-balanced binary search tree (BST). We explored the brute force approach, which involved using an array, and then optimized it by avoiding extra space and directly working with the linked list using an in-order traversal approach. We provided detailed implementations in multiple programming languages, ensuring that the solution is accessible to a wide range of developers. This problem not only reinforces the concept of linked lists and binary search trees but also highlights efficient space management and recursive thinking.

### Similar Questions

1. **Convert Sorted Array to Binary Search Tree**:
    - Given an array where elements are sorted in ascending order, convert it to a height-balanced BST.

2. **Flatten Binary Tree to Linked List**:
    - Given a binary tree, flatten it to a singly linked list in-place.

3. **Convert Binary Search Tree to Sorted Doubly Linked List**:
    - Convert a BST to a sorted circular doubly-linked list in-place.

4. **Balanced Binary Tree**:
    - Given a binary tree, determine if it is height-balanced.

5. **Kth Smallest Element in a BST**:
    - Given a binary search tree, write a function `kthSmallest` to find the `kth` smallest element in it.

6. **Inorder Successor in BST**:
    - Given a Binary Search Tree (BST) and a node, find the in-order successor of that node in the BST.

7. **Serialize and Deserialize Binary Tree**:
    - Design an algorithm to serialize and deserialize a binary tree.

These problems can help further solidify understanding of tree and linked list manipulations and enhance problem-solving skills in different scenarios.