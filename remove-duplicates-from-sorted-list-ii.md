### *Interviewer and Interviewee Discussion*

**Interviewer:** Let's discuss a problem where given the head of a sorted linked list, you need to delete all nodes that have duplicate numbers, leaving only distinct numbers from the original list. The result should also be sorted. Are you clear with the problem statement?

**Interviewee:** Yes, I understand. So we need to remove all nodes that have duplicates and return the remaining nodes in a sorted order. 

**Interviewer:** Correct. Can you start with explaining a brute force approach to solve this problem initially?

**Interviewee:** Sure. My initial thought for a brute-force approach would be to use nested loops to identify duplicates and then remove them. Here’s how I would think about it:

1. Traverse the linked list to collect the frequency of each value.
2. Use a hash map for counting each value's occurrence.
3. Traverse the list again to build a new list that only comprises nodes with a count of one.

**Interviewer:** That sounds like a plan. What would be the time and space complexity of your brute force approach?

**Interviewee:** 

- **Time Complexity:** The first traversal to collect the frequency takes O(n) time, and the second traversal to build the resultant list also takes O(n). Hence, the overall time complexity is O(2n) which simplifies to O(n).
- **Space Complexity:** We need extra space to store the frequencies of list elements, which could take up to O(n) space.

**Interviewer:** Good. Now let's discuss how we might optimize this approach. Could you think of any improvements or more efficient data structures?

**Interviewee:** To optimize, we can still utilize the fact that the input list is sorted. Here’s an optimized version:

1. Use a dummy node to handle edge cases gracefully and point to a new head.
2. Use two pointers: one to traverse the list (`current`) and one to build the new list (`prev`).
3. For each distinct value, check its neighbors to decide whether it should be included in the resulting list.

**Interviewer:** Can you detail this approach with some pseudocode and perhaps visualize it?

**Interviewee:** Absolutely.

- **Step-by-step Pseudocode:**
  1. Create a dummy node to ensure the head is handled correctly.
  2. Initialize two pointers, `prev` to the dummy and `current` to `head`.
  3. Traverse the list with `current`. If `current` and `current.next` have the same value, skip all the nodes with that value.
  4. If `current` does not have duplicates, move `prev` to `current`.
  5. Continue until the end of the list.

```python
class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

def deleteDuplicates(head: ListNode) -> ListNode:
    dummy = ListNode(0)
    dummy.next = head
    prev = dummy
    current = head
    
    while current:
        while current.next and current.val == current.next.val:
            current = current.next
        
        if prev.next == current:
            prev = prev.next
        else:
            prev.next = current.next
        
        current = current.next
    
    return dummy.next
```

#### Visualization:

Given list: [1,2,3,3,4,4,5]

Initial list:
```
dummy -> 1 -> 2 -> 3 -> 3 -> 4 -> 4 -> 5
```

After processing:
```
dummy -> 1 -> 2           5
           └─────────────>
```

Result list:
```
1 -> 2 -> 5
```

**Interviewer:** Excellent. This consolidated approach looks effective and you have also managed to keep the code clean while explaining the two-pointer technique. What’s the optimized time and space complexity here?

**Interviewee:** 
- **Time Complexity:** The optimized approach still traverses the list once, hence O(n).
- **Space Complexity:** We used a dummy node and two pointers, so the space complexity is O(1) (excluding the input list itself).
Sure, let's write the `deleteDuplicates` function for each language specified using the optimized approach discussed:

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
    ListNode* deleteDuplicates(ListNode* head) {
        if (!head) return nullptr;

        ListNode dummy(0, head);
        ListNode* prev = &dummy;
        ListNode* current = head;

        while (current) {
            while (current->next && current->val == current->next->val) {
                current = current->next;
            }

            if (prev->next == current) {
                prev = prev->next;
            } else {
                prev->next = current->next;
            }
            current = current->next;
        }

        return dummy.next;
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
    public ListNode deleteDuplicates(ListNode head) {
        if (head == null) return null;

        ListNode dummy = new ListNode(0, head);
        ListNode prev = dummy;
        ListNode current = head;

        while (current != null) {
            while (current.next != null && current.val == current.next.val) {
                current = current.next;
            }

            if (prev.next == current) {
                prev = prev.next;
            } else {
                prev.next = current.next;
            }
            current = current.next;
        }

        return dummy.next;
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
    def deleteDuplicates(self, head):
        """
        :type head: ListNode
        :rtype: ListNode
        """
        dummy = ListNode(0)
        dummy.next = head
        prev = dummy
        current = head
        
        while current:
            while current.next and current.val == current.next.val:
                current = current.next
            
            if prev.next == current:
                prev = prev.next
            else:
                prev.next = current.next
            
            current = current.next
        
        return dummy.next
```

### Python3

```python
# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next

class Solution:
    def deleteDuplicates(self, head: Optional[ListNode]) -> Optional[ListNode]:
        dummy = ListNode(0)
        dummy.next = head
        prev = dummy
        current = head
        
        while current:
            while current.next and current.val == current.next.val:
                current = current.next
            
            if prev.next == current:
                prev = prev.next
            else:
                prev.next = current.next
            
            current = current.next
        
        return dummy.next
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
struct ListNode* deleteDuplicates(struct ListNode* head) {
    struct ListNode dummy = {0, head};
    struct ListNode *prev = &dummy;
    struct ListNode *current = head;

    while (current) {
        while (current->next && current->val == current->next->val) {
            current = current->next;
        }

        if (prev->next == current) {
            prev = prev->next;
        } else {
            prev->next = current->next;
        }
        current = current->next;
    }

    return dummy.next;
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
    public ListNode DeleteDuplicates(ListNode head) {
        ListNode dummy = new ListNode(0, head);
        ListNode prev = dummy;
        ListNode current = head;

        while (current != null) {
            while (current.next != null && current.val == current.next.val) {
                current = current.next;
            }

            if (prev.next == current) {
                prev = prev.next;
            } else {
                prev.next = current.next;
            }
            current = current.next;
        }

        return dummy.next;
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
 * @return {ListNode}
 */
var deleteDuplicates = function(head) {
    let dummy = new ListNode(0, head);
    let prev = dummy;
    let current = head;

    while (current) {
        while (current.next && current.val === current.next.val) {
            current = current.next;
        }

        if (prev.next === current) {
            prev = prev.next;
        } else {
            prev.next = current.next;
        }
        current = current.next;
    }

    return dummy.next;
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

function deleteDuplicates(head: ListNode | null): ListNode | null {
  let dummy = new ListNode(0, head);
  let prev = dummy;
  let current = head;

  while (current) {
    while (current.next && current.val === current.next.val) {
      current = current.next;
    }

    if (prev.next === current) {
      prev = prev.next;
    } else {
      prev.next = current.next;
    }
    current = current.next;
  }

  return dummy.next;
}
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
     * @return ListNode
     */
    function deleteDuplicates($head) {
        $dummy = new ListNode(0);
        $dummy->next = $head;
        $prev = $dummy;
        $current = $head;

        while ($current) {
            while ($current->next && $current->val == $current->next->val) {
                $current = $current->next;
            }

            if ($prev->next == $current) {
                $prev = $prev->next;
            } else {
                $prev->next = $current->next;
            }
            $current = $current->next;
        }

        return $dummy->next;
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
    func deleteDuplicates(_ head: ListNode?) -> ListNode? {
        let dummy: ListNode? = ListNode(0)
        dummy?.next = head
        var prev = dummy
        var current = head
        
        while current != nil {
            while current?.next != nil && current?.val == current?.next?.val {
                current = current?.next
            }
            
            if prev?.next === current {
                prev = prev?.next
            } else {
                prev?.next = current?.next
            }
            current = current?.next
        }
        
        return dummy?.next
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
    fun deleteDuplicates(head: ListNode?): ListNode? {
        val dummy = ListNode(0)
        dummy.next = head
        var prev = dummy
        var current = head
        
        while (current != null) {
            while (current?.next != null && current.`val` == current.next?.`val`) {
                current = current.next
            }
            
            if (prev.next == current) {
                prev = prev.next!!
            } else {
                prev.next = current?.next
            }
            current = current?.next
        }
        
        return dummy.next
    }
}
```

### Dart

```dart
class ListNode {
  int val;
  ListNode? next;
  ListNode([this.val = 0, this.next]);
}

class Solution {
  ListNode? deleteDuplicates(ListNode? head) {
    ListNode dummy = ListNode(0, head);
    ListNode? prev = dummy;
    ListNode? current = head;

    while (current != null) {
      while (current!.next != null && current.val == current.next!.val) {
        current = current.next;
      }

      if (prev!.next == current) {
        prev = prev.next;
      } else {
        prev.next = current!.next;
      }
      current = current.next;
    }

    return dummy.next;
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
func deleteDuplicates(head *ListNode) *ListNode {
    dummy := &ListNode{Next: head}
    prev := dummy
    current := head
    
    for current != nil {
        for current.Next != nil && current.Val == current.Next.Val {
            current = current.Next
        }
        
        if prev.Next == current {
            prev = prev.Next
        } else {
            prev.Next = current.Next
        }
        current = current.Next
    }
    
    return dummy.Next
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
# @return {ListNode}
def delete_duplicates(head)
    dummy = ListNode.new(0)
    dummy.next = head
    prev = dummy
    current = head

    while current
        while current.next && current.val == current.next.val
            current = current.next
        end

        if prev.next == current
            prev = prev.next
        else
            prev.next = current.next
        end
        current = current.next
    end

    dummy.next
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
    def deleteDuplicates(head: ListNode): ListNode = {
        val dummy = new ListNode(0, head)
        var prev = dummy
        var current = head
        
        while (current != null) {
            while (current.next != null && current.x == current.next.x) {
                current = current.next
            }
            
            if (prev.next == current) {
                prev = prev.next
            } else {
                prev.next = current.next
            }
            current = current.next
        }
        
        dummy.next
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
    pub fn delete_duplicates(head: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
        let mut dummy = Box::new(ListNode { val: 0, next: head });
        let mut prev = &mut dummy;
        let mut current = prev.next.as_mut();

        while let Some(c) = current {
            while c.next.is_some() && c.val == c.next.as_ref().unwrap().val {
                c.next = c.next.as_mut().unwrap().next.take();
            }

            if prev.next.as_ref().unwrap().val == prev.next.as_ref().unwrap().val {
                prev = prev.next.as_mut().unwrap();
            } else {
                prev.next = c.next.take();
            }
            current = prev.next.as_mut();
        }

        dummy.next
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

(define/contract (delete-duplicates head)
  (-> (or/c list-node? #f) (or/c list-node? #f))
  )
```

### Erlang

```erlang
%% Definition for singly-linked list.
%%
%% -record(list_node, {val = 0 :: integer(),
%%                     next = null :: 'null' | #list_node{}}).

-spec delete_duplicates(Head :: #list_node{} | null) -> #list_node{} | null.
delete_duplicates(Head) ->

.
```

### Elixir

```elixir
# Definition for singly-linked list.
#
# defmodule ListNode do
#   @type t :: %__MODULE__{
#           val: integer,
#           next: ListNode.t() | nil
#         }
#   defstruct val: 0, next: nil
# end

defmodule Solution do
  @spec delete_duplicates(head :: ListNode.t | nil) :: ListNode.t | nil
  def delete_duplicates(head) do
    
  end
end
```


### Closing Statement

**Interviewer:** Excellent job! You've written a well-optimized solution for deleting duplicates from a sorted linked list and provided code implementations in various programming languages. This demonstration shows your ability to understand the problem, devise an efficient approach, and implement it across multiple language platforms. Well done!

**Interviewee:** Thank you! This was an interesting problem to solve and a great exercise in ensuring the solution works efficiently across different programming environments. I appreciate the opportunity and the chance to discuss and implement the solution.

**Interviewer:** Great! Keep practicing different data structures and algorithms as they form the backbone of many interview problems. Let's wrap up for today, and good luck with your preparations.

### Similar Questions

Here are some similar problems that could be useful to practice:

1. **Remove Duplicates from Sorted List**:
   - Given the head of a sorted linked list, delete all duplicates such that each element appears only once. Return the linked list sorted as well.

2. **Merge Two Sorted Lists**:
   - Merge two sorted linked lists and return it as a sorted list. The list should be made by splicing together the nodes of the first two lists.

3. **Reverse Linked List**:
   - Reverse a singly linked list.

4. **Remove Nth Node From End of List**:
   - Given a linked list, remove the nth node from the end of the list and return its head.

5. **Linked List Cycle**:
   - Given a linked list, determine if it has a cycle in it.

6. **Palindrome Linked List**:
   - Given the head of a singly linked list, return true if it is a palindrome.

7. **Intersection of Two Linked Lists**:
   - Write a program to find the node at which the intersection of two singly linked lists begins.

8. **Partition List**:
   - Given the head of a linked list and a value x, partition it such that all nodes less than x come before nodes greater than or equal to x.

9. **Rotate List**:
   - Given the head of a linked list, rotate the list to the right by k places.

10. **Flatten a Multilevel Doubly Linked List**:
    - You are given a doubly linked list, which contains nodes that also have a child property that might point to a separate doubly linked list. These child lists may have one or more children of their own, and so on, to produce a multilevel data structure. Flatten the list so that all the nodes appear in a single-level, doubly linked list.

These problems provide a diverse set of challenges for working with linked lists and can help further solidify your understanding and handling of this data structure.