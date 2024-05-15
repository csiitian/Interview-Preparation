## Interview Discussion

**Interviewer:** Let's discuss a problem on linked lists. Given the head of a sorted linked list, you need to delete all duplicates such that each element appears only once. You should return the linked list sorted as well. Here are a couple of examples for you to understand:

1. Input: [1,1,2]
   Output: [1,2]

2. Input: [1,1,2,3,3]
   Output: [1,2,3]
   
The constraints are:
- The number of nodes is in the range [0, 300].
- Node values range from -100 to 100.
- The list is guaranteed to be sorted in ascending order.

**Interviewee:** Got it. The list being sorted helps a lot with removing duplicates. I can iterate through the list and whenever I find consecutive nodes with the same value, I can skip the duplicate nodes. 

**Interviewer:** Sounds good. Could you describe your initial thoughts on a brute force approach?

**Interviewee:** Sure. For a brute force approach:
1. We would traverse the entire linked list.
2. At each node, we check if the next node has the same value.
3. If it does, we skip the next node by linking the current node to the node after the duplicate.
4. We continue this process until we traverse the entire list.

In terms of time complexity, since we are iterating through the linked list once, it would be O(n), where n is the number of nodes in the list.

For space complexity, we aren't using any additional space apart from a few pointers, so it's O(1).

**Interviewer:** That's a good start. Let's put this into code and then, if needed, we can think about optimizations.

## Code Implementation

```python
class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

def deleteDuplicates(head: ListNode) -> ListNode:
    current = head
    
    while current and current.next:
        if current.val == current.next.val:
            current.next = current.next.next  # Skip the duplicate
        else:
            current = current.next  # Move to the next distinct element
    
    return head
```

**Interviewee:** This solution iterates through the list and removes duplicates by skipping over them.

**Interviewer:** Great. Since the problem states that the input list is already sorted, this approach is both time and space efficient. Could you explain with a diagram how this works?

**Interviewee:** Sure. Let's take the second example for explanation:

### Example: [1, 1, 2, 3, 3]

1. Start with `head` pointing to the first node (1).
2. Compare `head` value (1) with `head.next` value (1).
    - Since they are equal, link the first 1 to the node after next, which is 2.
3. Now, `head` will be:
    - 1 -> 2 -> 3 -> 3
4. Move to the next distinct element (2).
5. Compare 2 with the next value (3).
    - Since they are different, just move to the next node.
6. Now, `head` will be:
    - 1 -> 2 -> 3 -> 3
7. Move to the next element (3) and compare with next value (also 3).
    - Since they are equal, link 3 to the node after next which is `None`.
8. Now, `head` will be:
    - 1 -> 2 -> 3

The final list is [1, 2, 3], with duplicates removed.

Here is a visual representation of this:

```
Initial List: 1 -> 1 -> 2 -> 3 -> 3
Steps:
1. Compare nodes:
   1 ->  1 -> 2 -> 3 -> 3
   ^     ^
   |___skip duplicate____|
2. List becomes:
   1 -> 2 -> 3 -> 3
3. Continue with next nodes:
   1 ->  2 -> 3 -> 3
           ^    ^
           |___skip duplicate____|
4. Final list:
   1 -> 2 -> 3
```
**Interviewer:** Excellent! This explanation and visual representation make it very clear. Your approach is effective for this problem.
Sure! Let's go through each of the languages and provide the code for deleting duplicates in a sorted linked list while also commenting on the time and space complexities.

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
        ListNode* current = head;
        while (current && current->next) {
            if (current->val == current->next->val) {
                current->next = current->next->next;  // Skip duplicate
            } else {
                current = current->next;
            }
        }
        return head;
    }
};

// Time Complexity: O(n), where n is the number of nodes in the list.
// Space Complexity: O(1), no additional space is used apart from variables.
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
        ListNode current = head;
        while (current != null && current.next != null) {
            if (current.val == current.next.val) {
                current.next = current.next.next;  // Skip duplicate
            } else {
                current = current.next;
            }
        }
        return head;
    }
}

// Time Complexity: O(n), where n is the number of nodes in the list.
// Space Complexity: O(1), no additional space is used apart from variables.
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
        current = head
        while current and current.next:
            if current.val == current.next.val:
                current.next = current.next.next  # Skip duplicate
            else:
                current = current.next
        return head

# Time Complexity: O(n), where n is the number of nodes in the list.
# Space Complexity: O(1), no additional space is used apart from variables.
```

### Python 3

```python
# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next

from typing import Optional

class Solution:
    def deleteDuplicates(self, head: Optional[ListNode]) -> Optional[ListNode]:
        current = head
        while current and current.next:
            if current.val == current.next.val:
                current.next = current.next.next  # Skip duplicate
            else:
                current = current.next
        return head

# Time Complexity: O(n), where n is the number of nodes in the list.
# Space Complexity: O(1), no additional space is used apart from variables.
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
    struct ListNode* current = head;
    while (current && current->next) {
        if (current->val == current->next->val) {
            current->next = current->next->next;  // Skip duplicate
        } else {
            current = current->next;
        }
    }
    return head;
}

// Time Complexity: O(n), where n is the number of nodes in the list.
// Space Complexity: O(1), no additional space is used apart from variables.
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
        ListNode current = head;
        while (current != null && current.next != null) {
            if (current.val == current.next.val) {
                current.next = current.next.next;  // Skip duplicate
            } else {
                current = current.next;
            }
        }
        return head;
    }
}

// Time Complexity: O(n), where n is the number of nodes in the list.
// Space Complexity: O(1), no additional space is used apart from variables.
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
    let current = head;
    while (current !== null && current.next !== null) {
        if (current.val === current.next.val) {
            current.next = current.next.next;  // Skip duplicate
        } else {
            current = current.next;
        }
    }
    return head;
};

// Time Complexity: O(n), where n is the number of nodes in the list.
// Space Complexity: O(1), no additional space is used apart from variables.
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
    let current = head;
    while (current !== null && current.next !== null) {
        if (current.val === current.next.val) {
            current.next = current.next.next;  // Skip duplicate
        } else {
            current = current.next;
        }
    }
    return head;
}

// Time Complexity: O(n), where n is the number of nodes in the list.
// Space Complexity: O(1), no additional space is used apart from variables.
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
        $current = $head;
        while ($current !== null && $current->next !== null) {
            if ($current->val === $current->next->val) {
                $current->next = $current->next->next;  // Skip duplicate
            } else {
                $current = $current->next;
            }
        }
        return $head;
    }
}

// Time Complexity: O(n), where n is the number of nodes in the list.
// Space Complexity: O(1), no additional space is used apart from variables.
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
        var current = head
        while current != nil && current?.next != nil {
            if current?.val == current?.next?.val {
                current?.next = current?.next?.next  // Skip duplicate
            } else {
                current = current?.next
            }
        }
        return head
    }
}

// Time Complexity: O(n), where n is the number of nodes in the list.
// Space Complexity: O(1), no additional space is used apart from variables.
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
        var current = head
        while (current != null && current.next != null) {
            if (current.`val` == current.next?.`val`) {
                current.next = current.next?.next  // Skip duplicate
            } else {
                current = current.next
            }
        }
        return head
    }
}

// Time Complexity: O(n), where n is the number of nodes in the list.
// Space Complexity: O(1), no additional space is used apart from variables.
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
  ListNode? deleteDuplicates(ListNode? head) {
    ListNode? current = head;
    while (current != null && current.next != null) {
      if (current.val == current.next?.val) {
        current.next = current.next?.next;  // Skip duplicate
      } else {
        current = current.next;
      }
    }
    return head;
  }
}

// Time Complexity: O(n), where n is the number of nodes in the list.
// Space Complexity: O(1), no additional space is used apart from variables.
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
    current := head
    for current != nil && current.Next != nil {
        if current.Val == current.Next.Val {
            current.Next = current.Next.Next  // Skip duplicate
        } else {
            current = current.Next
        }
    }
    return head
}

// Time Complexity: O(n), where n is the number of nodes in the list.
// Space Complexity: O(1), no additional space is used apart from variables.
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
    current = head
    while current && current.next
        if current.val == current.next.val
            current.next = current.next.next  # Skip duplicate
        else
            current = current.next
        end
    end
    head
end

# Time Complexity: O(n), where n is the number of nodes in the list.
# Space Complexity: O(1), no additional space is used apart from variables.
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
        var current = head
        while (current != null && current.next != null) {
            if (current.x == current.next.x) {
                current.next = current.next.next  // Skip duplicate
            } else {
                current = current.next
            }
        }
        head
    }
}

// Time Complexity: O(n), where n is the number of nodes in the list.
// Space Complexity: O(1), no additional space is used apart from variables.
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
        let mut current = head.as_mut();
        while let Some(ref mut node) = current {
            while let Some(ref next) = node.next {
                if next.val == node.val {
                    node.next = next.next.clone();  // Skip duplicate
                } else {
                    break;
                }
            }
            current = node.next.as_mut();
        }
        head
    }
}

// Time Complexity: O(n), where n is the number of nodes in the list.
// Space Complexity: O(1), no additional space is used apart from variables.
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
  (let loop ([current head])
    (cond
      [(null? current) head]
      [(null? (list-node-next current)) head]
      [(= (list-node-val current) (list-node-val (list-node-next current)))
       (let ([next-next (list-node-next (list-node-next current))])
         (set-list-node-next! current next-next))]
      [else (loop (list-node-next current))]
      )
    head
    )
  )

; Time Complexity: O(n), where n is the number of nodes in the list.
; Space Complexity: O(1), no additional space is used apart from variables.
```

### Erlang

```erlang
%% Definition for singly-linked list.
%%
%% -record(list_node, {val = 0 :: integer(),
%%                     next = null :: 'null' | #list_node{}}).

-spec delete_duplicates(Head :: #list_node{} | null) -> #list_node{} | null.
delete_duplicates(Head) ->
    delete_duplicates(Head, Head).

delete_duplicates(null, _) ->
    null;
delete_duplicates(#list_node{next = null} = Head, _) ->
    Head;
delete_duplicates(#list_node{val = Val, next = #list_node{val = Val}} = Head, #list_node{} = PreservedHead) ->
    delete_duplicates(Head#list_node{next = Head#list_node.next#list_node.next}, PreservedHead);
delete_duplicates(#list_node{next = #list_node{} = Next} = Head, #list_node{} = PreservedHead) ->
    delete_duplicates(Next, Head).

% Time Complexity: O(n), where n is the number of nodes in the list.
% Space Complexity: O(1), no additional space is used apart from variables.
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
    delete_duplicates(head, head)
  end

  defp delete_duplicates(nil, _preserve_head), do: nil
  defp delete_duplicates(%ListNode{next: nil} = head, _preserve_head), do: head
  defp delete_duplicates(%ListNode{val: val, next: %ListNode{val: val}} = head, preserve_head) do
    delete_duplicates(%{head | next: head.next.next}, preserve_head)
  end
  defp delete_duplicates(%ListNode{next: %ListNode{} = next} = head, preserve_head) do
    delete_duplicates(next, head)
  end
end

# Time Complexity: O(n), where n is the number of nodes in the list.
# Space Complexity: O(1), no additional space is used apart from variables.
```

Note that the core logic remains the same for all these languages, with only syntactical differences. The comments mention the time and space complexities, which are O(n) and O(1) respectively.
## Closing Statement

**Interviewer:** Excellent work! You have implemented the solution to remove duplicates from a sorted linked list in various programming languages efficiently. You've correctly identified that the key advantage here is the list being sorted, which simplifies the problem to a linear scan with O(n) time complexity and O(1) space complexity. Your explanations and the provided code are clear and in-depth. This solution demonstrates a solid understanding of linked list operations and efficient algorithm design.

**Interviewee:** Thank you! This was a great exercise in understanding how to approach problems involving linked lists and practice writing clean, efficient code in multiple languages. I'm glad we covered both the brute force and optimal approaches, and I appreciate the discussion on time and space complexity.

**Interviewer:** Great. Let's wrap up with some similar questions that you might find useful for further practice.

### Similar Questions

1. **Remove Duplicates from Sorted Array**
   - Problem: Given a sorted array `nums`, remove the duplicates in-place such that each element appears only once and returns the new length.
   - [Link to Problem](https://leetcode.com/problems/remove-duplicates-from-sorted-array/)

2. **Remove Duplicates from Unsorted Linked List**
   - Problem: Given an unsorted linked list, remove all duplicates while maintaining the original order of elements.

3. **Intersection of Two Linked Lists**
   - Problem: Given the heads of two singly linked lists, return the node at which the two lists intersect. If the two linked lists have no intersection, return null.
   - [Link to Problem](https://leetcode.com/problems/intersection-of-two-linked-lists/)

4. **Merge Two Sorted Lists**
   - Problem: Merge two sorted linked lists and return them as a sorted list. The list should be made by splicing together the nodes of the first two lists.
   - [Link to Problem](https://leetcode.com/problems/merge-two-sorted-lists/)

5. **Linked List Cycle**
   - Problem: Given a linked list, determine if it has a cycle in it.
   - [Link to Problem](https://leetcode.com/problems/linked-list-cycle/)

6. **Reverse Linked List**
   - Problem: Reverse a singly linked list.
   - [Link to Problem](https://leetcode.com/problems/reverse-linked-list/)

7. **Remove Nth Node From End of List**
   - Problem: Given the head of a linked list, remove the nth node from the end of the list and return its head.
   - [Link to Problem](https://leetcode.com/problems/remove-nth-node-from-end-of-list/)

8. **Palindrome Linked List**
   - Problem: Given a singly linked list, determine if it is a palindrome.
   - [Link to Problem](https://leetcode.com/problems/palindrome-linked-list/)

9. **Sort List**
   - Problem: Sort a linked list in O(n log n) time using constant space complexity.
   - [Link to Problem](https://leetcode.com/problems/sort-list/)

10. **Split Linked List in Parts**
    - Problem: Given the head of a singly linked list and an integer k, split the linked list into k consecutive linked list parts.
    - [Link to Problem](https://leetcode.com/problems/split-linked-list-in-parts/)

Continue practicing these problems to solidify your understanding of linked list operations and improve your problem-solving skills. Good luck!