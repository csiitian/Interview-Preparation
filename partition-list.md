### Interviewer and Interviewee Discussion

**Interviewer:** Let's discuss a problem where you're given the head of a linked list and a value `x`. You need to partition the linked list such that all nodes with values less than `x` come before nodes with values greater than or equal to `x`. Also, you should preserve the original relative order of the nodes in each partition. How would you approach this?

**Interviewee:** Sure, that sounds interesting. I think we can start by thinking about a basic brute force approach first.

### Initial Thoughts on the Brute Force Approach

**Interviewee:** In the brute force approach, I would consider the following steps:
1. Traverse the entire linked list.
2. Create two new linked lists – one for nodes less than `x` and the other for nodes greater than or equal to `x`.
3. Append nodes to the respective new linked lists as per their values.
4. Finally, concatenate the two lists, with the "less than" list coming before the "greater than or equal" list.

**Interviewer:** That sounds sensible. Could you talk about the time and space complexity for this brute force approach?

**Interviewee:** Sure.
- **Time Complexity:** We traverse the linked list once to partition the nodes into two lists. This traversal takes O(n) time, where n is the number of nodes in the linked list.
- **Space Complexity:** We use extra space to store the two new linked lists. In the worst case, this would be O(n) space because we are storing nodes in two new lists.

**Interviewer:** Okay, let's move on to optimizing this approach. How can we improve either the time or space complexity?

### Optimizing the Brute Force Approach

**Interviewee:** To optimize our approach, we can actually maintain four pointers instead of creating new linked lists:
1. Two pointers for the start and end of the "less than" list.
2. Two pointers for the start and end of the "greater than or equal to" list.
3. We will traverse the linked list once and rearrange the pointers to partition the list on-the-fly.
4. Finally, we concatenate the two lists.

This way, we can maintain the original linked list structure and avoid using extra space for new lists.

Here's an illustration to explain it better:

```
Original Linked List: 1 -> 4 -> 3 -> 2 -> 5 -> 2
Partition value (x): 3

During the traversal, two separate sections of pointers are managed:

"Less than x" list pointers:
1 -> 2 -> 2

"Greater than or equal to x" list pointers:
4 -> 3 -> 5

Concatenate the two lists:
1 -> 2 -> 2 -> 4 -> 3 -> 5
```

### Code Implementation

```python
class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

def partition(head: ListNode, x: int) -> ListNode:
    less_head = ListNode(0)  # Placeholder for the starting point of "less than" list
    greater_head = ListNode(0)  # Placeholder for the starting point of "greater or equal" list
    
    less = less_head  # Pointer to construct "less than" list
    greater = greater_head  # Pointer to construct "greater or equal" list
    
    current = head
    
    while current:
        if current.val < x:
            less.next = current
            less = less.next
        else:
            greater.next = current
            greater = greater.next
        current = current.next
    
    greater.next = None  # End the greater or equal list
    less.next = greater_head.next  # Concatenate the less list and the greater or equal list

    return less_head.next  # Return the head of the combined linked list
```

### Time and Space Complexity for Optimized Approach

**Time Complexity:**
- This approach still traverses the linked list once, so the time complexity remains O(n).

**Space Complexity:**
- Instead of using extra lists, we rearrange pointers. Thus, the space complexity is O(1) as we're not using extra space proportional to the input size.

**Interviewer:** Excellent. You not only approached the problem step by step but also optimized it efficiently while maintaining original constraints. Well done!
Certainly! Below, I've written the code to implement the optimized approach for partitioning a linked list in various programming languages, with each wrapped in the provided method skeleton.

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
class Solution {
public:
    ListNode* partition(ListNode* head, int x) {
        ListNode* lessHead = new ListNode(0);
        ListNode* greaterHead = new ListNode(0);
        ListNode* less = lessHead;
        ListNode* greater = greaterHead;

        while (head != nullptr) {
            if (head->val < x) {
                less->next = head;
                less = less->next;
            } else {
                greater->next = head;
                greater = greater->next;
            }
            head = head->next;
        }

        greater->next = nullptr;
        less->next = greaterHead->next;
        return lessHead->next;
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
class Solution {
    public ListNode partition(ListNode head, int x) {
        ListNode lessHead = new ListNode(0);
        ListNode greaterHead = new ListNode(0);
        ListNode less = lessHead;
        ListNode greater = greaterHead;

        while (head != null) {
            if (head.val < x) {
                less.next = head;
                less = less.next;
            } else {
                greater.next = head;
                greater = greater.next;
            }
            head = head.next;
        }

        greater.next = null;
        less.next = greaterHead.next;
        return lessHead.next;
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
class Solution(object):
    def partition(self, head, x):
        """
        :type head: ListNode
        :type x: int
        :rtype: ListNode
        """
        less_head = ListNode(0)
        greater_head = ListNode(0)
        less = less_head
        greater = greater_head

        while head:
            if head.val < x:
                less.next = head
                less = less.next
            else:
                greater.next = head
                greater = greater.next
            head = head.next

        greater.next = None
        less.next = greater_head.next
        return less_head.next
```

### Python3
```python
# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next
class Solution:
    def partition(self, head: Optional[ListNode], x: int) -> Optional[ListNode]:
        less_head = ListNode(0)
        greater_head = ListNode(0)
        less = less_head
        greater = greater_head

        while head:
            if head.val < x:
                less.next = head
                less = less.next
            else:
                greater.next = head
                greater = greater.next
            head = head.next

        greater.next = None
        less.next = greater_head.next
        return less_head.next
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
struct ListNode* partition(struct ListNode* head, int x) {
    struct ListNode* lessHead = (struct ListNode*)malloc(sizeof(struct ListNode));
    struct ListNode* greaterHead = (struct ListNode*)malloc(sizeof(struct ListNode));
    struct ListNode* less = lessHead;
    struct ListNode* greater = greaterHead;

    lessHead->next = NULL;
    greaterHead->next = NULL;

    while (head != NULL) {
        if (head->val < x) {
            less->next = head;
            less = less->next;
        } else {
            greater->next = head;
            greater = greater->next;
        }
        head = head->next;
    }

    greater->next = NULL;
    less->next = greaterHead->next;
    return lessHead->next;
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
public class Solution {
    public ListNode Partition(ListNode head, int x) {
        ListNode lessHead = new ListNode(0);
        ListNode greaterHead = new ListNode(0);
        ListNode less = lessHead;
        ListNode greater = greaterHead;

        while (head != null) {
            if (head.val < x) {
                less.next = head;
                less = less.next;
            } else {
                greater.next = head;
                greater = greater.next;
            }
            head = head.next;
        }

        greater.next = null;
        less.next = greaterHead.next;
        return lessHead.next;
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
 * @param {ListNode} head
 * @param {number} x
 * @return {ListNode}
 */
var partition = function(head, x) {
    let lessHead = new ListNode(0);
    let greaterHead = new ListNode(0);
    let less = lessHead;
    let greater = greaterHead;

    while (head !== null) {
        if (head.val < x) {
            less.next = head;
            less = less.next;
        } else {
            greater.next = head;
            greater = greater.next;
        }
        head = head.next;
    }

    greater.next = null;
    less.next = greaterHead.next;
    return lessHead.next;
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

function partition(head: ListNode | null, x: number): ListNode | null {
    let lessHead = new ListNode(0);
    let greaterHead = new ListNode(0);
    let less = lessHead;
    let greater = greaterHead;

    while (head !== null) {
        if (head.val < x) {
            less.next = head;
            less = less.next;
        } else {
            greater.next = head;
            greater = greater.next;
        }
        head = head.next;
    }

    greater.next = null;
    less.next = greaterHead.next;
    return lessHead.next;
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
class Solution {

    /**
     * @param ListNode $head
     * @param Integer $x
     * @return ListNode
     */
    function partition($head, $x) {
        $lessHead = new ListNode(0);
        $greaterHead = new ListNode(0);
        $less = $lessHead;
        $greater = $greaterHead;

        while ($head != null) {
            if ($head->val < $x) {
                $less->next = $head;
                $less = $less->next;
            } else {
                $greater->next = $head;
                $greater = $greater->next;
            }
            $head = $head->next;
        }

        $greater->next = null;
        $less->next = $greaterHead->next;
        return $lessHead->next;
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
class Solution {
    func partition(_ head: ListNode?, _ x: Int) -> ListNode? {
        let lessHead = ListNode(0)
        let greaterHead = ListNode(0)
        var less = lessHead
        var greater = greaterHead

        var current = head

        while current != nil {
            if current!.val < x {
                less.next = current
                less = less.next!
            } else {
                greater.next = current
                greater = greater.next!
            }
            current = current!.next
        }

        greater.next = nil
        less.next = greaterHead.next
        return lessHead.next
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
class Solution {
    fun partition(head: ListNode?, x: Int): ListNode? {
        val lessHead = ListNode(0)
        val greaterHead = ListNode(0)
        var less = lessHead
        var greater = greaterHead

        var current = head

        while (current != null) {
            if (current.`val` < x) {
                less.next = current
                less = less.next!!
            } else {
                greater.next = current
                greater = greater.next!!
            }
            current = current.next
        }

        greater.next = null
        less.next = greaterHead.next
        return lessHead.next
    }
}
```

### Dart
```dart
/**
 * Definition for singly-linked list.
 * class ListNode {
 *   int val;
 *   ListNode? next;
 *   ListNode([this.val = 0, this.next]);
 * }
 */
class Solution {
  ListNode? partition(ListNode? head, int x) {
    ListNode lessHead = ListNode(0);
    ListNode greaterHead = ListNode(0);
    ListNode less = lessHead;
    ListNode greater = greaterHead;

    ListNode? current = head;

    while (current != null) {
      if (current.val < x) {
        less.next = current;
        less = less.next!;
      } else {
        greater.next = current;
        greater = greater.next!;
      }
      current = current.next;
    }

    greater.next = null;
    less.next = greaterHead.next;
    return lessHead.next;
  }
}
```

### Go
```go
/**
 * Definition for singly-linked list.
 * type ListNode struct {
 *     Val int
 *     Next *ListNode
 * }
 */
func partition(head *ListNode, x int) *ListNode {
    lessHead := &ListNode{}
    greaterHead := &ListNode{}
    less := lessHead
    greater := greaterHead

    for head != nil {
        if head.Val < x {
            less.Next = head
            less = less.Next
        } else {
            greater.Next = head
            greater = greater.Next
        }
        head = head.Next
    }

    greater.Next = nil
    less.Next = greaterHead.Next
    return lessHead.Next
}
```

### Ruby
```ruby
# Definition for singly-linked list.
# class ListNode
#     attr_accessor :val, :next
#     def initialize(val = 0, _next = nil)
#         @val = val
#         @next = _next
#     end
# end
# @param {ListNode} head
# @param {Integer} x
# @return {ListNode}
def partition(head, x)
    less_head = ListNode.new(0)
    greater_head = ListNode.new(0)
    less = less_head
    greater = greater_head

    current = head

    while current != nil
        if current.val < x
            less.next = current
            less = less.next
        else
            greater.next = current
            greater = greater.next
        end
        current = current.next
    end

    greater.next = nil
    less.next = greater_head.next
    return less_head.next
end
```

### Scala
```scala
/**
 * Definition for singly-linked list.
 * class ListNode(_x: Int = 0, _next: ListNode = null) {
 *   var next: ListNode = _next
 *   var x: Int = _x
 * }
 */
object Solution {
    def partition(head: ListNode, x: Int): ListNode = {
        val lessHead = new ListNode(0)
        val greaterHead = new ListNode(0)
        var less = lessHead
        var greater = greaterHead

        var current = head

        while (current != null) {
            if (current.x < x) {
                less.next = current
                less = less.next
            } else {
                greater.next = current
                greater = greater.next
            }
            current = current.next
        }

        greater.next = null
        less.next = greaterHead.next
        lessHead.next
    }
}
```

### Rust
```rust
// Definition for singly-linked list.
// #[derive(PartialEq, Eq, Clone, Debug)]
// pub struct ListNode {
//   pub val: i32,
//   pub next: Option<Box<ListNode>>
// }
// 
// impl ListNode {
//   #[inline]
//   fn new(val: i32) -> Self {
//     ListNode {
//       next: None,
//       val
//     }
//   }
// }
impl Solution {
    pub fn partition(head: Option<Box<ListNode>>, x: i32) -> Option<Box<ListNode>> {
        let mut less_head = Box::new(ListNode::new(0));
        let mut greater_head = Box::new(ListNode::new(0));
        let mut less = &mut less_head;
        let mut greater = &mut greater_head;

        let mut current = head;
        while let Some(mut node) = current {
            if node.val < x {
                less.next = Some(node);
                less = less.next.as_mut().unwrap();
            } else {
                greater.next = Some(node);
                greater = greater.next.as_mut().unwrap();
            }
            current = less.next.take().or(greater.next.take());
        }

        greater.next = None;
        less.next = greater_head.next;
        less_head.next
    }
}
```

### Racket
```racket
; Definition for singly-linked list:
#|

; val : integer?
; next : (or/c list-node? #f)
(struct list-node
  (val next) #:mutable #:transparent)

; constructor
(define (make-list-node [val 0])
  (list-node val #f))

|#

(define/contract (partition head x)
  (-> (or/c list-node? #f) exact-integer? (or/c list-node? #f))
  (define less-head (make-list-node 0))
  (define greater-head (make-list-node 0))
  (define less less-head)
  (define greater greater-head)

  (define current head)
  (while (not (equal? current #f))
    (if (< (list-node-val current) x)
        (begin
          (set-list-node-next! less current)
          (set! less (list-node-next less)))
        (begin
          (set-list-node-next! greater current)
          (set! greater (list-node-next greater))))
    (set! current (list-node-next current)))

  (set-list-node-next! greater #f)
  (set-list-node-next! less (list-node-next greater-head))
  (list-node-next less-head))
```

### Erlang
```erlang
%% Definition for singly-linked list.
%%
%% -record(list_node, {val = 0 :: integer(),
%%                     next = null :: 'null' | #list_node{}}).

-spec partition(Head :: #list_node{} | null, X :: integer()) -> #list_node{} | null.
partition(Head, X) ->
    LessHead = #list_node{val = 0},
    GreaterHead = #list_node{val = 0},
    Less = case LessHead of
        #list_node{} -> LessHead;
        null -> null
    end,
    Greater = case GreaterHead of
        #list_node{} -> GreaterHead;
        null -> null
    end,

    Current = Head,
    while (if Current =/= null -> true; true -> false) of
        (if Current#list_node.val < X -> true; true -> false) of
            true ->
                Less#list_node.next = Current,
                Less = Less#list_node.next;
            false ->
                Greater#list_node.next = Current,
                Greater = Greater#list_node.next
        end,
        Current = Current#list_node.next
    end,

    Greater#list_node.next = null,
    Less#list_node.next = GreaterHead#list_node.next,
    LessHead#list_node.next.
```


   
### Closing Statement

**Interviewer:** Great job! You did an excellent job breaking down the problem step by step. Starting with a brute force approach and then optimizing it was the right approach. You provided a clear and efficient solution and implemented it across multiple programming languages accurately. You've demonstrated a strong understanding of both linked lists and algorithm optimization.

**Interviewee:** Thank you! I enjoyed this problem, and I'm glad I could implement it efficiently. I appreciate the opportunity to explore solutions in different languages as well.

**Interviewer:** You're welcome. Keep practicing similar problems; it's a great way to enhance your problem-solving skills. Let's discuss some more questions that you might encounter in interviews, which require similar analytical thinking.

### Similar Questions

1. **Remove Linked List Elements:** Given the `head` of a linked list and an integer `val`, remove all the nodes of the linked list that have `Node.val` equal to `val`, and return the new head.
   
2. **Odd Even Linked List:** Given a singly linked list, group all odd-indexed nodes together followed by the even-indexed nodes. Ensure the relative order inside both the odd and even groups is maintained.

3. **Reverse Linked List:** Reverse a singly linked list.

4. **Reorder List:** Given a singly linked list `L : L0 → L1 → … → Ln-1 → Ln`, reorder it to: `L0 → Ln → L1 → Ln-1 → L2 → Ln-2 → …`

5. **Merge Two Sorted Lists:** Merge two sorted linked lists and return it as a new sorted list.

6. **Linked List Cycle:** Given `head`, determine whether the linked list has a cycle in it.

7. **Remove Duplicates from Sorted List:** Given a sorted linked list, delete all duplicates such that each element appears only once.

8. **Linked List Cycle II:** Given a linked list, return the node where the cycle begins. If there is no cycle, return `null`.

9. **Rotate List:** Given the `head` of a linked list, rotate the list to the right by `k` places.

10. **Intersection of Two Linked Lists:** Write a program to find the node at which the intersection of two singly linked lists begins.

These problems will help you practice various operations on linked lists, like modifying the structure, handling edge cases, and ensuring optimal performance. Good luck with your preparation!