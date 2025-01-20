### Interviewer and Interviewee Discussion

**Interviewer**: We have a problem here. Given the head of a linked list and an integer value, you need to remove all nodes from the list that have the given value. The result should be the new head of the list. Let's discuss how you would approach this problem.

**Interviewee**: Sure, let's break down the problem. Essentially, we need to traverse the linked list and remove all nodes that match the given value.

**Interviewer**: That's correct. Can you explain to me a brute force approach you would take to solve this?

**Interviewee**: For a brute force approach, we could simply iterate through the list with a pointer. As we traverse, we check if each node's value matches the given value. If it does, we skip the node by adjusting the `next` pointer of the previous node to skip the current node.

**Interviewer**: Alright, let's formalize that. 

1. We'll need to handle the edge case where the list is empty.
2. We'll use a dummy node that points to the head of the list to handle cases where the head itself needs to be removed.
3. Traverse the list using a pointer. If a node matches the given value, we'll adjust the pointers to exclude it.

**Interviewee**: Exactly. This helps us avoid special cases for handling the head of the list. Once we complete the traversal, we can return the next node of our dummy node as the new head of the list.

**Interviewer**: Sounds good. What about the time and space complexity of this approach?

**Interviewee**: For the brute force approach:
- **Time Complexity**: O(n), where n is the number of nodes in the linked list. We only need to traverse the list once.
- **Space Complexity**: O(1), because we are using a constant amount of extra space (just a few pointers).

**Interviewer**: Great. This brute force approach is efficient in terms of time and space. Let's move on to optimizing it, if possible.

### Optimized Approach

**Interviewee**: In this case, the brute force approach is already quite optimal. The operations required (traversing and adjusting pointers) are minimal. However, we can ensure the code is clean and handles all edge cases properly.

**Interviewer**: Agreed. Let's take a look at a possible implementation. 

```python
class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

def removeElements(head: ListNode, val: int) -> ListNode:
    # Create a dummy node that points to the list head.
    dummy = ListNode(next=head)
    current = dummy

    while current.next:
        if current.next.val == val:
            # Skip the node with the matching value.
            current.next = current.next.next
        else:
            # Move to the next node.
            current = current.next

    # The new head of the list is the next node of our dummy node.
    return dummy.next
```

### Diagram Explanation

To help visualize, let's use a diagram with an example list and value. Suppose we have `head = [1, 2, 6, 3, 4, 5, 6]` and `val = 6`.

1. Initial list: `1 -> 2 -> 6 -> 3 -> 4 -> 5 -> 6`
2. Add a dummy node: `D -> 1 -> 2 -> 6 -> 3 -> 4 -> 5 -> 6`
3. Traverse the list and skip nodes equal to 6:

```
D -> 1 -> 2    6 -> 3 -> 4 -> 5 -> 6
D -> 1 -> 2 -> 3    4 -> 5 -> 6
D -> 1 -> 2 -> 3 -> 4 -> 5    6
D -> 1 -> 2 -> 3 -> 4 -> 5
```

4. Return `dummy.next` which is `1`.

This approach ensures a clean and effective way to remove elements from the list.

**Interviewer**: Excellent. This covers the edge cases and maintains an efficient complexity. I think this approach would work well for the given constraints. Thank you for the explanation.

**Interviewee**: You're welcome!
Sure, let's start by filling in the method for removing elements from the linked list for each language provided. The core logic will remain the same as discussed before.

### 1. C++

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
    ListNode* removeElements(ListNode* head, int val) {
        ListNode* dummy = new ListNode(0, head);
        ListNode* current = dummy;

        while (current->next != nullptr) {
            if (current->next->val == val) {
                ListNode* temp = current->next;
                current->next = current->next->next;
                delete temp;
            } else {
                current = current->next;
            }
        }

        ListNode* newHead = dummy->next;
        delete dummy;
        return newHead;
    }
};
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### 2. Java

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
    public ListNode removeElements(ListNode head, int val) {
        ListNode dummy = new ListNode(0);
        dummy.next = head;
        ListNode current = dummy;

        while (current.next != null) {
            if (current.next.val == val) {
                current.next = current.next.next;
            } else {
                current = current.next;
            }
        }

        return dummy.next;
    }
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### 3. Python

```python
# Definition for singly-linked list.
# class ListNode(object):
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next

class Solution(object):
    def removeElements(self, head, val):
        """
        :type head: ListNode
        :type val: int
        :rtype: ListNode
        """
        dummy = ListNode(0)
        dummy.next = head
        current = dummy

        while current.next:
            if current.next.val == val:
                current.next = current.next.next
            else:
                current = current.next
        
        return dummy.next
# Time Complexity: O(n)
# Space Complexity: O(1)
```

### 4. Python3

```python
# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next

class Solution:
    def removeElements(self, head: Optional[ListNode], val: int) -> Optional[ListNode]:
        dummy = ListNode(0)
        dummy.next = head
        current = dummy

        while current.next:
            if current.next.val == val:
                current.next = current.next.next
            else:
                current = current.next
        
        return dummy.next
# Time Complexity: O(n)
# Space Complexity: O(1)
```

### 5. C

```c
/**
 * Definition for singly-linked list.
 * struct ListNode {
 *     int val;
 *     struct ListNode *next;
 * };
 */
struct ListNode* removeElements(struct ListNode* head, int val) {
    struct ListNode dummy;
    dummy.next = head;
    struct ListNode* current = &dummy;

    while (current->next != NULL) {
        if (current->next->val == val) {
            struct ListNode* temp = current->next;
            current->next = current->next->next;
            free(temp);
        } else {
            current = current->next;
        }
    }

    return dummy.next;
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### 6. C#

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
    public ListNode RemoveElements(ListNode head, int val) {
        ListNode dummy = new ListNode(0);
        dummy.next = head;
        ListNode current = dummy;

        while (current.next != null) {
            if (current.next.val == val) {
                current.next = current.next.next;
            } else {
                current = current.next;
            }
        }

        return dummy.next;
    }
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### 7. JavaScript

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
 * @param {number} val
 * @return {ListNode}
 */
var removeElements = function(head, val) {
    let dummy = new ListNode(0);
    dummy.next = head;
    let current = dummy;

    while (current.next !== null) {
        if (current.next.val === val) {
            current.next = current.next.next;
        } else {
            current = current.next;
        }
    }

    return dummy.next;
};
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### 8. TypeScript

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

function removeElements(head: ListNode | null, val: number): ListNode | null {
    const dummy = new ListNode(0);
    dummy.next = head;
    let current = dummy;

    while (current.next !== null) {
        if (current.next.val === val) {
            current.next = current.next.next;
        } else {
            current = current.next;
        }
    }

    return dummy.next;
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### 9. PHP

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
     * @param Integer $val
     * @return ListNode
     */
    function removeElements($head, $val) {
        $dummy = new ListNode(0);
        $dummy->next = $head;
        $current = $dummy;

        while ($current->next != null) {
            if ($current->next->val == $val) {
                $current->next = $current->next->next;
            } else {
                $current = $current->next;
            }
        }

        return $dummy->next;
    }
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### 10. Swift

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
    func removeElements(_ head: ListNode?, _ val: Int) -> ListNode? {
        let dummy = ListNode(0)
        dummy.next = head
        var current = dummy

        while current.next != nil {
            if current.next!.val == val {
                current.next = current.next!.next
            } else {
                current = current.next!
            }
        }

        return dummy.next
    }
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### 11. Kotlin

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
    fun removeElements(head: ListNode?, `val`: Int): ListNode? {
        val dummy = ListNode(0)
        dummy.next = head
        var current = dummy

        while (current.next != null) {
            if (current.next!!.`val` == `val`) {
                current.next = current.next!!.next
            } else {
                current = current.next!!
            }
        }

        return dummy.next
    }
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### 12. Dart

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
  ListNode? removeElements(ListNode? head, int val) {
    final dummy = ListNode(0, head);
    var current = dummy;

    while (current.next != null) {
      if (current.next!.val == val) {
        current.next = current.next!.next;
      } else {
        current = current.next!;
      }
    }

    return dummy.next;
  }
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### 13. Go

```go
/**
 * Definition for singly-linked list.
 * type ListNode struct {
 *     Val int
 *     Next *ListNode
 * }
 */
func removeElements(head *ListNode, val int) *ListNode {
    dummy := &ListNode{Next: head}
    current := dummy

    for current.Next != nil {
        if current.Next.Val == val {
            current.Next = current.Next.Next
        } else {
            current = current.Next
        }
    }

    return dummy.Next
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### 14. Ruby

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
# @param {Integer} val
# @return {ListNode}
def remove_elements(head, val)
    dummy = ListNode.new(0, head)
    current = dummy

    while current.next
        if current.next.val == val
            current.next = current.next.next
        else
            current = current.next
        end
    end

    dummy.next
end
# Time Complexity: O(n)
# Space Complexity: O(1)
```

### 15. Scala

```scala
/**
 * Definition for singly-linked list.
 * class ListNode(_x: Int = 0, _next: ListNode = null) {
 *   var next: ListNode = _next
 *   var x: Int = _x
 * }
 */
object Solution {
    def removeElements(head: ListNode, `val`: Int): ListNode = {
        val dummy = new ListNode(0)
        dummy.next = head
        var current = dummy

        while (current.next != null) {
            if (current.next.x == `val`) {
                current.next = current.next.next
            } else {
                current = current.next
            }
        }

        dummy.next
    }
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### 16. Rust

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
    pub fn remove_elements(head: Option<Box<ListNode>>, val: i32) -> Option<Box<ListNode>> {
        let mut dummy = Box::new(ListNode {
            val: 0,
            next: head
        });
        let mut current = &mut dummy;

        while let Some(ref mut next) = current.next {
            if next.val == val {
                current.next = next.next.take();
            } else {
                current = current.next.as_mut().unwrap();
            }
        }

        dummy.next
    }
}
// Time Complexity: O(n)
// Space Complexity: O(1)
```

### 17. Racket

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

(define/contract (remove-elements head val)
  (-> (or/c list-node? #f) exact-integer? (or/c list-node? #f))
  (define dummy (make-list-node 0))
  (set-list-node-next! dummy head)
  (define current dummy)

  (let loop ()
    (when (list-node-next current)
      (if (= (list-node-val (list-node-next current)) val)
          (set-list-node-next! current (list-node-next (list-node-next current)))
          (set! current (list-node-next current))))
    (when (list-node-next current) (loop)))
  (list-node-next dummy))
  ; Time Complexity: O(n)
  ; Space Complexity: O(1)
```

### 18. Erlang

```erlang
%% Definition for singly-linked list.
%%
%% -record(list_node, {val = 0 :: integer(),
%%                     next = null :: 'null' | #list_node{}}).

-spec remove_elements(Head :: #list_node{} | null, Val :: integer()) -> #list_node{} | null.
remove_elements(Head, Val) ->
  Dummy = #list_node{val = 0, next = Head},
  Current = Dummy,
  remove_elements(Current, Val, Dummy#list_node.next).

remove_elements(Current, Val, null) ->
  Current#list_node.next;
remove_elements(Current, Val, NextNode) ->
  case NextNode#list_node.val of
    Val ->
      Current#list_node.next = NextNode#list_node.next,
      remove_elements(Current, Val, Current#list_node.next);
    _ ->
      remove_elements(NextNode, Val, NextNode#list_node.next)
  end.
% Time Complexity: O(n)
% Space Complexity: O(1)
```

### 19. Elixir

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
  @spec remove_elements(head :: ListNode.t | nil, val :: integer) :: ListNode.t | nil
  def remove_elements(head, val) do
    dummy = %ListNode{val: 0, next: head}
    current = dummy

    remove_elements_recursive(current, val)
    dummy.next
  end

  defp remove_elements_recursive(%ListNode{next: nil} = _current, _val), do: :ok

  defp remove_elements_recursive(%ListNode{} = current, val) do
    if current.next != nil and current.next.val == val do
      current.next = current.next.next
    else
      remove_elements_recursive(current.next, val)
    end
  end
end
# Time Complexity: O(n)
# Space Complexity: O(1)
```

Each of these implementations ensures that we traverse the list only once, removing nodes with the matching value, which provides a time complexity of O(n) and a space complexity of O(1).


### Closing Statement

This concludes our discussion on removing elements from a linked list given a specific value. We started by understanding the problem and discussing an initial brute force approach. We then analyzed its time and space complexity and confirmed that it is already quite optimal, both in terms of time (O(n)) and space (O(1)). We proceeded to implement this approach in various programming languages, ensuring our solution is both efficient and covers all edge cases. Throughout the discussion, we made sure our solution is easy to understand and straightforward to implement.

### Similar Questions

Here are some related questions that can help further practice and solidify the understanding of linked list manipulations:

1. **Reverse a Linked List (LeetCode #206)**:
   * Problem: Reverse a singly linked list.
   * Challenge: Reverse the list in-place with O(1) extra space and O(n) time complexity.

2. **Remove Nth Node From End of List (LeetCode #19)**:
   * Problem: Given a linked list, remove the nth node from the end of the list and return its head.
   * Challenge: Do this in one pass.

3. **Merge Two Sorted Lists (LeetCode #21)**:
   * Problem: Merge two sorted linked lists and return it as a new sorted list.
   * Challenge: Implement an efficient merge algorithm.

4. **Palindrome Linked List (LeetCode #234)**:
   * Problem: Determine if a linked list is a palindrome.
   * Challenge: Accomplish this with O(n) time and O(1) extra space.

5. **Linked List Cycle (LeetCode #141)**:
   * Problem: Given a linked list, determine if it has a cycle in it.
   * Challenge: Solve this problem using Floyd's Cycle detection algorithm, also known as the Tortoise and Hare algorithm.

6. **Intersection of Two Linked Lists (LeetCode #160)**:
   * Problem: Find the node at which the intersection of two singly linked lists begins.
   * Challenge: Ensure the solution runs in O(n) time and uses O(1) space.

These questions not only hone your skills with linked lists but also ensure you are comfortable with various common operations and edge cases associated with them. Keep practicing and good luck with your coding journey!