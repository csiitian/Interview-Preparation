### Interviewer and Interviewee Discussion

**Interviewer:** So, here is the problem statement. You are given a linked list where each node has an additional random pointer which could point to any node in the list or `null`. You need to create a deep copy of this linked list. Do you understand the problem?

**Interviewee:** Yes, I understand the problem. We need to create a new linked list with the same structure and values as the original list, including both the `next` and `random` pointers. None of the pointers in the new list should reference any nodes from the original list.

**Interviewer:** Good. What are your initial thoughts on how to solve this problem? Any brute force approach that comes to mind?

### Initial Thoughts & Brute Force Approach

**Interviewee:** Initially, we could think about creating new nodes for each node in the original list and then setting up the `next` pointers correctly. Once that is complete, we would go through the original list again to set up the `random` pointers for the new nodes.

1. **Create new nodes and setup the `next` pointers:**
   - Traverse the original list and for each node, create a corresponding new node.
   - Maintain a mapping from original nodes to new nodes using a hashmap.

2. **Set up the `random` pointers:**
   - Traverse the original list again, and using the hashmap, set up the `random` pointers for the new nodes.

**Interviewer:** That sounds like a plan. Can you walk me through the time and space complexity of this approach?

### Time and Space Complexity of Brute Force

**Interviewee:**
- **Time Complexity:**
  - Creating new nodes with the correct `next` pointers would take `O(n)` time where `n` is the number of nodes in the linked list.
  - Setting up the `random` pointers would also take `O(n)` time as we are iterating through the list again.
  - Therefore, the total time complexity would be `O(n) + O(n) = O(n)`.

- **Space Complexity:**
  - We use a hashmap to store the mapping from original nodes to new nodes. This hashmap would store `n` key-value pairs.
  - Hence, the space complexity is `O(n)` for the hashmap.

### Optimization with More Efficient Data Structure

**Interviewer:** Great. Is there a way to optimize the space complexity?

**Interviewee:** Yes, we can optimize space complexity by interleaving the original and copied nodes. Here’s how:

1. **Interleaving the Nodes:**
   - Traverse the original list and for each original node, create a new node and insert it next to the original node.
   - This way, the new nodes are interleaved with the original nodes.

2. **Setting Up Random Pointers:**
   - Traverse the interleaved list and set up the `random` pointers for the new nodes. Use the original node’s `random` pointer to set the corresponding new node’s `random` pointer.

3. **Restoring the List and Extracting the Copied List:**
   - Separate the copied nodes from the original nodes to form the final deep-copied list.
   
Here's a step-by-step visual representation:

#### Step 1: Interleaving the Nodes
```
Original: A -> B -> C
After Interleaving: A -> A' -> B -> B' -> C -> C'
```

#### Step 2: Setting Up Random Pointers
```
If A.random = C
then set A'.random = C'
```

#### Step 3: Restoring the Lists
```
Separate the interleaved list:
Original: A -> B -> C
Copied: A' -> B' -> C'
```

**Interviewer:** That’s efficient in terms of space. Can you summarize the complexity for this approach?

**Interviewee:**

- **Time Complexity:** 
  - Interleaving the nodes takes `O(n)`.
  - Setting up the random pointers also takes `O(n)`.
  - Separating the lists, again, takes `O(n)`.
  - Overall time complexity is `O(n)`.

- **Space Complexity:** 
  - We do not use any extra space apart from a few pointers, so the space complexity is `O(1)`.

**Interviewer:** Excellent. You have successfully optimized the solution both in terms of time and space. Great job!
Sure! Below are the solutions with time and space complexity in all the provided languages:

### C++
```cpp
/*
// Definition for a Node.
class Node {
public:
    int val;
    Node* next;
    Node* random;
    
    Node(int _val) {
        val = _val;
        next = NULL;
        random = NULL;
    }
};
*/

class Solution {
public:
    Node* copyRandomList(Node* head) {
        if (!head) return nullptr;

        Node* current = head;

        // Step 1: Interleave the copied nodes with original nodes
        while (current) {
            Node* copy = new Node(current->val);
            copy->next = current->next;
            current->next = copy;
            current = copy->next;
        }

        // Step 2: Assign random pointers for the copied nodes
        current = head;
        while (current) {
            if (current->random) {
                current->next->random = current->random->next;
            }
            current = current->next->next;
        }

        // Step 3: Separate the copied list from the original list
        current = head;
        Node* copyHead = head->next;
        Node* copyCurrent = copyHead;

        while (current) {
            current->next = current->next->next;
            if (copyCurrent->next) {
                copyCurrent->next = copyCurrent->next->next;
            }
            current = current->next;
            copyCurrent = copyCurrent->next;
        }

        return copyHead;
    }
};
```

### Java
```java
/*
// Definition for a Node.
class Node {
    int val;
    Node next;
    Node random;

    public Node(int val) {
        this.val = val;
        this.next = null;
        this.random = null;
    }
}
*/

class Solution {
    public Node copyRandomList(Node head) {
        if (head == null) return null;

        Node current = head;

        // Step 1: Interleave the copied nodes with original nodes
        while (current != null) {
            Node copy = new Node(current.val);
            copy.next = current.next;
            current.next = copy;
            current = copy.next;
        }

        // Step 2: Assign random pointers for the copied nodes
        current = head;
        while (current != null) {
            if (current.random != null) {
                current.next.random = current.random.next;
            }
            current = current.next.next;
        }

        // Step 3: Separate the copied list from the original list
        current = head;
        Node copyHead = head.next;
        Node copyCurrent = copyHead;

        while (current != null) {
            current.next = current.next.next;
            if (copyCurrent.next != null) {
                copyCurrent.next = copyCurrent.next.next;
            }
            current = current.next;
            copyCurrent = copyCurrent.next;
        }

        return copyHead;
    }
}
```

### Python
```python
"""
# Definition for a Node.
class Node:
    def __init__(self, x, next=None, random=None):
        self.val = int(x)
        self.next = next
        self.random = random
"""

class Solution(object):
    def copyRandomList(self, head):
        """
        :type head: Node
        :rtype: Node
        """
        if not head:
            return None

        current = head

        # Step 1: Interleave the copied nodes with original nodes
        while current:
            copy = Node(current.val)
            copy.next = current.next
            current.next = copy
            current = copy.next

        # Step 2: Assign random pointers for the copied nodes
        current = head
        while current:
            if current.random:
                current.next.random = current.random.next
            current = current.next.next

        # Step 3: Separate the copied list from the original list
        current = head
        copy_head = head.next
        copy_current = copy_head

        while current:
            current.next = current.next.next
            if copy_current.next:
                copy_current.next = copy_current.next.next
            current = current.next
            copy_current = copy_current.next

        return copy_head
```

### Python3
```python
"""
# Definition for a Node.
class Node:
    def __init__(self, x: int, next: 'Node' = None, random: 'Node' = None):
        self.val = int(x)
        self.next = next
        self.random = random
"""

class Solution:
    def copyRandomList(self, head: 'Optional[Node]') -> 'Optional[Node]':
        if not head:
            return None

        current = head

        # Step 1: Interleave the copied nodes with original nodes
        while current:
            copy = Node(current.val)
            copy.next = current.next
            current.next = copy
            current = copy.next

        # Step 2: Assign random pointers for the copied nodes
        current = head
        while current:
            if current.random:
                current.next.random = current.random.next
            current = current.next.next

        # Step 3: Separate the copied list from the original list
        current = head
        copy_head = head.next
        copy_current = copy_head

        while current:
            current.next = current.next.next
            if copy_current.next:
                copy_current.next = copy_current.next.next
            current = current.next
            copy_current = copy_current.next

        return copy_head
```

### C
```c
/**
 * Definition for a Node.
 * struct Node {
 *     int val;
 *     struct Node *next;
 *     struct Node *random;
 * };
 */

struct Node* copyRandomList(struct Node* head) {
    if (!head) return NULL;

    struct Node* current = head;

    // Step 1: Interleave the copied nodes with original nodes
    while (current) {
        struct Node* copy = (struct Node*)malloc(sizeof(struct Node));
        copy->val = current->val;
        copy->next = current->next;
        current->next = copy;
        current = copy->next;
    }

    // Step 2: Assign random pointers for the copied nodes
    current = head;
    while (current) {
        if (current->random) {
            current->next->random = current->random->next;
        }
        current = current->next->next;
    }

    // Step 3: Separate the copied list from the original list
    current = head;
    struct Node* copyHead = head->next;
    struct Node* copyCurrent = copyHead;

    while (current) {
        current->next = current->next->next;
        if (copyCurrent->next) {
            copyCurrent->next = copyCurrent->next->next;
        }
        current = current->next;
        copyCurrent = copyCurrent->next;
    }

    return copyHead;
}
```

### C#
```csharp
/*
// Definition for a Node.
public class Node {
    public int val;
    public Node next;
    public Node random;
    
    public Node(int _val) {
        val = _val;
        next = null;
        random = null;
    }
}
*/

public class Solution {
    public Node CopyRandomList(Node head) {
        if (head == null) return null;

        Node current = head;

        // Step 1: Interleave the copied nodes with original nodes
        while (current != null) {
            Node copy = new Node(current.val);
            copy.next = current.next;
            current.next = copy;
            current = copy.next;
        }

        // Step 2: Assign random pointers for the copied nodes
        current = head;
        while (current != null) {
            if (current.random != null) {
                current.next.random = current.random.next;
            }
            current = current.next.next;
        }

        // Step 3: Separate the copied list from the original list
        current = head;
        Node copyHead = head.next;
        Node copyCurrent = copyHead;

        while (current != null) {
            current.next = current.next.next;
            if (copyCurrent.next != null) {
                copyCurrent.next = copyCurrent.next.next;
            }
            current = current.next;
            copyCurrent = copyCurrent.next;
        }

        return copyHead;
    }
}
```

### JavaScript
```javascript
/**
 * // Definition for a Node.
 * function Node(val, next, random) {
 *    this.val = val;
 *    this.next = next;
 *    this.random = random;
 * };
 */

/**
 * @param {Node} head
 * @return {Node}
 */
var copyRandomList = function(head) {
    if (!head) return null;

    let current = head;

    // Step 1: Interleave the copied nodes with original nodes
    while (current) {
        let copy = new Node(current.val);
        copy.next = current.next;
        current.next = copy;
        current = copy.next;
    }

    // Step 2: Assign random pointers for the copied nodes
    current = head;
    while (current) {
        if (current.random) {
            current.next.random = current.random.next;
        }
        current = current.next.next;
    }

    // Step 3: Separate the copied list from the original list
    current = head;
    let copyHead = head.next;
    let copyCurrent = copyHead;

    while (current) {
        current.next = current.next.next;
        if (copyCurrent.next) {
            copyCurrent.next = copyCurrent.next.next;
        }
        current = current.next;
        copyCurrent = copyCurrent.next;
    }

    return copyHead;
};
```

### TypeScript
```typescript
/**
 * Definition for Node.
 * class Node {
 *     val: number
 *     next: Node | null
 *     random: Node | null
 *     constructor(val?: number, next?: Node, random?: Node) {
 *         this.val = (val===undefined ? 0 : val)
 *         this.next = (next===undefined ? null : next)
 *         this.random = (random===undefined ? null : random)
 *     }
 * }
 */

function copyRandomList(head: Node | null): Node | null {
    if (!head) return null;

    let current: Node | null = head;

    // Step 1: Interleave the copied nodes with original nodes
    while (current) {
        let copy = new Node(current.val);
        copy.next = current.next;
        current.next = copy;
        current = copy.next;
    }

    // Step 2: Assign random pointers for the copied nodes
    current = head;
    while (current) {
        if (current.random) {
            current.next.random = current.random.next;
        }
        current = current.next.next;
    }

    // Step 3: Separate the copied list from the original list
    current = head;
    let copyHead = head.next;
    let copyCurrent = copyHead;

    while (current) {
        current.next = current.next.next;
        if (copyCurrent.next) {
            copyCurrent.next = copyCurrent.next.next;
        }
        current = current.next;
        copyCurrent = copyCurrent.next;
    }

    return copyHead;
}
```

### PHP
```php
/**
 * Definition for a Node.
 * class Node {
 *     public $val = null;
 *     public $next = null;
 *     public $random = null;
 *     function __construct($val = 0) {
 *         $this->val = $val;
 *         $this->next = null;
 *         $this->random = null;
 *     }
 * }
 */

class Solution {
    /**
     * @param Node $head
     * @return Node
     */
    function copyRandomList($head) {
        if ($head === null) return null;

        $current = $head;

        // Step 1: Interleave the copied nodes with original nodes
        while ($current !== null) {
            $copy = new Node($current->val);
            $copy->next = $current->next;
            $current->next = $copy;
            $current = $copy->next;
        }

        // Step 2: Assign random pointers for the copied nodes
        $current = $head;
        while ($current !== null) {
            if ($current->random !== null) {
                $current->next->random = $current->random->next;
            }
            $current = $current->next->next;
        }

        // Step 3: Separate the copied list from the original list
        $current = $head;
        $copyHead = $head->next;
        $copyCurrent = $copyHead;

        while ($current !== null) {
            $current->next = $current->next->next;
            if ($copyCurrent->next !== null) {
                $copyCurrent->next = $copyCurrent->next->next;
            }
            $current = $current->next;
            $copyCurrent = $copyCurrent->next;
        }

        return $copyHead;
    }
}
```

### Swift
```swift
/**
 * Definition for a Node.
 * public class Node {
 *     public var val: Int
 *     public var next: Node?
 *     public var random: Node?
 *     public init(_ val: Int) {
 *         self.val = val
 *         self.next = nil
 *   	   self.random = nil
 *     }
 * }
 */

class Solution {
    func copyRandomList(_ head: Node?) -> Node? {
        if head == nil { return nil }

        var current = head

        // Step 1: Interleave the copied nodes with original nodes
        while current != nil {
            let copy = Node(current!.val)
            copy.next = current!.next
            current!.next = copy
            current = copy.next
        }

        // Step 2: Assign random pointers for the copied nodes
        current = head
        while current != nil {
            if let random = current!.random {
                current!.next!.random = random.next
            }
            current = current!.next!.next
        }

        // Step 3: Separate the copied list from the original list
        current = head
        let copyHead = head!.next
        var copyCurrent = copyHead

        while current != nil {
            current!.next = current!.next!.next
            if copyCurrent!.next != nil {
                copyCurrent!.next = copyCurrent!.next!.next
            }
            current = current!.next
            copyCurrent = copyCurrent!.next
        }

        return copyHead
    }
}
```

### Kotlin
```kotlin
/**
 * Example:
 * var ti = Node(5)
 * var v = ti.`val`
 * Definition for a Node.
 * class Node(var `val`: Int) {
 *     var next: Node? = null
 *     var random: Node? = null
 * }
 */

class Solution {
    fun copyRandomList(node: Node?): Node? {
        if (node == null) return null

        var current = node

        // Step 1: Interleave the copied nodes with original nodes
        while (current != null) {
            val copy = Node(current.`val`)
            copy.next = current.next
            current.next = copy
            current = copy.next
        }

        // Step 2: Assign random pointers for the copied nodes
        current = node
        while (current != null) {
            if (current.random != null) {
                current.next!!.random = current.random!!.next
            }
            current = current.next!!.next
        }

        // Step 3: Separate the copied list from the original list
        current = node
        val copyHead = node.next
        var copyCurrent = copyHead

        while (current != null) {
            current.next = current.next!!.next
            if (copyCurrent!!.next != null) {
                copyCurrent.next = copyCurrent.next!!.next
            }
            current = current.next
            copyCurrent = copyCurrent.next
        }

        return copyHead
    }
}
```

### Go
```go
/**
 * Definition for a Node.
 * type Node struct {
 *     Val int
 *     Next *Node
 *     Random *Node
 * }
 */

func copyRandomList(head *Node) *Node {
    if head == nil {
        return nil
    }

    current := head

    // Step 1: Interleave the copied nodes with original nodes
    for current != nil {
        copy := &Node{Val: current.Val}
        copy.Next = current.Next
        current.Next = copy
        current = copy.Next
    }

    // Step 2: Assign random pointers for the copied nodes
    current = head
    for current != nil {
        if current.Random != nil {
            current.Next.Random = current.Random.Next
        }
        current = current.Next.Next
    }

    // Step 3: Separate the copied list from the original list
    current = head
    copyHead := head.Next
    copyCurrent := copyHead

    for current != nil {
        current.Next = current.Next.Next
        if copyCurrent.Next != nil {
            copyCurrent.Next = copyCurrent.Next.Next
        }
        current = current.Next
        copyCurrent = copyCurrent.Next
    }

    return copyHead
}
```

### Ruby
```ruby
# Definition for Node.
# class Node
#     attr_accessor :val, :next, :random
#     def initialize(val = 0)
#         @val = val
#         @next = nil
#	   @random = nil
#     end
# end

# @param {Node} node
# @return {Node}
def copy_random_list(head)
    return nil if head.nil?

    current = head

    # Step 1: Interleave the copied nodes with original nodes
    while current
        copy = Node.new(current.val)
        copy.next = current.next
        current.next = copy
        current = copy.next
    end

    # Step 2: Assign random pointers for the copied nodes
    current = head
    while current
        if current.random
            current.next.random = current.random.next
        end
        current = current.next.next
    end

    # Step 3: Separate the copied list from the original list
    current = head
    copy_head = head.next
    copy_current = copy_head

    while current
        current.next = current.next.next
        if copy_current.next
            copy_current.next = copy_current.next.next
        end
        current = current.next
        copy_current = copy_current.next
    end

    copy_head
end
```


### Closing Statement

**Interviewer:** Great job! You’ve implemented the solution in multiple languages, optimized for both time and space complexity. Your approach of interleaving copied nodes and resolving pointers in a second pass is efficient and well thought out. This method ensures that the deep copy maintains the exact structure and links of the original list without unnecessarily using extra space. Your understanding and application of the algorithmic concepts are commendable. 

**Interviewee:** Thank you! It was a valuable exercise in understanding deep copies and managing complex pointer structures in a linked list. I appreciate the opportunity to work through the problem and optimize the solution.

**Interviewer:** You're welcome. As you continue to prepare, consider practicing similar problems to reinforce these concepts. Here are some additional related questions for practice.

### Similar Questions

1. **Copy List with Random Pointer II:** How would you extend the solution if each node has multiple random pointers instead of just one?
2. **Flatten a Multilevel Doubly Linked List:** Given a doubly linked list where in addition to the next and previous pointers, each node has a child pointer, which may or may not point to a separate doubly linked list. These child lists may have one or more children of their own, and so on, to produce a multilevel data structure. Flatten the list so that all the nodes appear in a single-level, doubly linked list.
3. **Detect a Cycle in a Linked List:** Given a linked list, determine if it has a cycle in it. Can you do it without using extra space?
4. **Reverse a Linked List:** Reverse a singly linked list.
5. **Merge Two Sorted Linked Lists:** Merge two sorted linked lists and return it as a new sorted list. The new list should be made by splicing together the nodes of the first two lists.
6. **Linked List Random Node:** Given a singly linked list, return a random node's value with equal probability from the linked list.
7. **LRU Cache Implementation:** Design and implement a data structure for Least Recently Used (LRU) cache.

These questions will help you sharpen your skills in linked lists and other related data structures. Keep practicing and good luck!

**Interviewee:** Thank you for the suggestions! I'll definitely work on these problems to further improve my understanding and problem-solving skills.