### Interviewer and Interviewee Discussion

**Interviewer:** Let's delve into the problem. We have a linked list, and we're tasked with removing the n-th node from the end of the list. Can you explain your initial thoughts on how to approach this problem?

**Interviewee:** Sure, let's start off with a brute force approach. To remove the n-th node from the end, we can break the problem into two main steps:
1. Determine the total length of the linked list.
2. Calculate the position of the node to remove from the beginning of the list, which is \( \text{length} - n + 1 \).
3. Traverse the list again to actually remove the node.

**Interviewer:** That sounds like a reasonable plan. Let's break it down further. How would you implement the step where you calculate the total length of the linked list?

**Interviewee:** We can initialize a counter to zero and iterate through the list, incrementing the counter on each step until we reach the end of the list.

**Interviewer:** Great. How about the second step then?

**Interviewee:** For the second step, after calculating the position from the beginning, we can traverse the list again until we reach that position and adjust the pointers to remove the node.

### Brute Force Approach

**Interviewer:** Can you describe the brute force approach in more detail?

**Interviewee:** Absolutely. Here's the algorithm:
1. Initialize `length = 0`.
2. Traverse the linked list to compute its `length`.
3. Compute the position to remove from the start: \( \text{node\_to\_remove} = \text{length} - n + 1 \).
4. Traverse the list again to find and remove the node at position `node_to_remove`.
5. Adjust the pointers accordingly to remove the node from the list.

**Interviewer:** Perfect, now let's talk about the time and space complexity of this approach.

**Interviewee:** The time complexity consists of two traversals:
- Computing the length: \( O(\text{sz}) \)
- Finding and removing the node: \( O(\text{sz}) \)

Thus, the total time complexity is \( O(\text{sz} + \text{sz}) = O(\text{sz}) \).

The space complexity is \( O(1) \) since we're using a fixed number of additional variables, irrespective of the size of the input list.

### Optimized Approach

**Interviewer:** Can you think of a way to optimize this approach, maybe even avoiding the need for two passes?

**Interviewee:** Yes, we could use a two-pointer technique to accomplish this in one pass. We can set up two pointers: `first` and `second`. We move the `first` pointer `n` steps forward in the list, then move both `first` and `second` simultaneously until the `first` pointer reaches the end of the list. At this point, the `second` pointer will be at the node just before the one to be removed.

**Interviewer:** That sounds promising. Can you describe the steps and the justification?

**Interviewee:** Sure. Here’s the plan:
1. Initialize two pointers, `first` and `second`, both pointing to the head.
2. Move `first` pointer `n` nodes ahead.
3. Move both pointers together until `first` reaches the end of the list.
4. `second` will now be pointing to the (n+1)-th node from the end. We can adjust the pointers to remove the n-th node.

### Drawing
Let's visualize this:
Consider the list [1, 2, 3, 4, 5] and `n = 2`.

```
Initial:   1 -> 2 -> 3 -> 4 -> 5
           ^              ^
           |              |
         second          first
```

Move `first` `n` steps ahead.

```
After n steps: 1 -> 2 -> 3 -> 4 -> 5
                          ^     
                          |
                        first

         second still at head  
```

Move both pointers together:

```
           1 -> 2 -> 3 -> 4 -> 5
                 ^              ^
                 |              |
               second         first
               
           1 -> 2 -> 3 -> 4 -> 5
                     ^              ^
                     |              |
                   second         first
           
           1 -> 2 -> 3 -> 4 -> 5
                          ^              ^
                          |              |
                        second         first (end)
```

At this point, `second` points to the node just before the one we want to remove.

### Algorithm Implementation

```python
class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

def removeNthFromEnd(head: ListNode, n: int) -> ListNode:
    dummy = ListNode(0)
    dummy.next = head
    first = dummy
    second = dummy
    
    for _ in range(n + 1):
        first = first.next
    
    while first is not None:
        first = first.next
        second = second.next
    
    second.next = second.next.next
    
    return dummy.next
```

### Complexity Analysis

**Interviewer:** What about the time and space complexity of this optimized approach?

**Interviewee:**
- **Time Complexity:** \( O(\text{sz}) \). We only traverse the list once.
- **Space Complexity:** \( O(1) \). We are using a constant amount of extra space.

**Interviewer:** Excellent work. This solution is efficient and avoids the need for multiple passes through the list.
Absolutely! Let's implement the optimized approach for the "Remove N-th Node From End of List" problem in each of the provided languages, along with the time and space complexity.

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
    ListNode* removeNthFromEnd(ListNode* head, int n) {
        ListNode* dummy = new ListNode(0);
        dummy->next = head;
        ListNode* first = dummy;
        ListNode* second = dummy;
        
        // Move first pointer n+1 steps ahead
        for (int i = 0; i <= n; ++i) {
            first = first->next;
        }
        
        // Move both pointers until first reaches the end
        while (first != nullptr) {
            first = first->next;
            second = second->next;
        }
        
        // Remove the nth node from the end
        second->next = second->next->next;
        
        return dummy->next;
    }
};

// Time Complexity: O(sz)
// Space Complexity: O(1)
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
    public ListNode removeNthFromEnd(ListNode head, int n) {
        ListNode dummy = new ListNode(0);
        dummy.next = head;
        ListNode first = dummy;
        ListNode second = dummy;
        
        // Move first pointer n+1 steps ahead
        for (int i = 0; i <= n; i++) {
            first = first.next;
        }
        
        // Move both pointers until first reaches the end
        while (first != null) {
            first = first.next;
            second = second.next;
        }
        
        // Remove the nth node from the end
        second.next = second.next.next;
        
        return dummy.next;
    }
}

// Time Complexity: O(sz)
// Space Complexity: O(1)
```

### Python

```python
# Definition for singly-linked list.
# class ListNode(object):
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next
class Solution(object):
    def removeNthFromEnd(self, head, n):
        """
        :type head: ListNode
        :type n: int
        :rtype: ListNode
        """
        dummy = ListNode(0)
        dummy.next = head
        first = dummy
        second = dummy
        
        # Move first pointer n+1 steps ahead
        for _ in range(n + 1):
            first = first.next
        
        # Move both pointers until first reaches the end
        while first is not None:
            first = first.next
            second = second.next
        
        # Remove the nth node from the end
        second.next = second.next.next
        
        return dummy.next

# Time Complexity: O(sz)
# Space Complexity: O(1)
```

### Python3

```python
# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next
class Solution:
    def removeNthFromEnd(self, head: Optional[ListNode], n: int) -> Optional[ListNode]:
        dummy = ListNode(0)
        dummy.next = head
        first = dummy
        second = dummy
        
        # Move first pointer n+1 steps ahead
        for _ in range(n + 1):
            first = first.next
        
        # Move both pointers until first reaches the end
        while first is not None:
            first = first.next
            second = second.next
        
        # Remove the nth node from the end
        second.next = second.next.next
        
        return dummy.next

# Time Complexity: O(sz)
# Space Complexity: O(1)
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
struct ListNode* removeNthFromEnd(struct ListNode* head, int n) {
    struct ListNode dummy;
    dummy.val = 0;
    dummy.next = head;
    struct ListNode* first = &dummy;
    struct ListNode* second = &dummy;
    
    // Move first pointer n+1 steps ahead
    for (int i = 0; i <= n; i++) {
        first = first->next;
    }
    
    // Move both pointers until first reaches the end
    while (first != NULL) {
        first = first->next;
        second = second->next;
    }
    
    // Remove the nth node from the end
    second->next = second->next->next;
    
    return dummy.next;
}

// Time Complexity: O(sz)
// Space Complexity: O(1)
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
    public ListNode RemoveNthFromEnd(ListNode head, int n) {
        ListNode dummy = new ListNode(0);
        dummy.next = head;
        ListNode first = dummy;
        ListNode second = dummy;
        
        // Move first pointer n+1 steps ahead
        for (int i = 0; i <= n; i++) {
            first = first.next;
        }
        
        // Move both pointers until first reaches the end
        while (first != null) {
            first = first.next;
            second = second.next;
        }
        
        // Remove the nth node from the end
        second.next = second.next.next;
        
        return dummy.next;
    }
}

// Time Complexity: O(sz)
// Space Complexity: O(1)
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
 * @param {number} n
 * @return {ListNode}
 */
var removeNthFromEnd = function(head, n) {
    let dummy = new ListNode(0);
    dummy.next = head;
    let first = dummy;
    let second = dummy;
    
    // Move first pointer n+1 steps ahead
    for (let i = 0; i <= n; i++) {
        first = first.next;
    }
    
    // Move both pointers until first reaches the end
    while (first !== null) {
        first = first.next;
        second = second.next;
    }
    
    // Remove the nth node from the end
    second.next = second.next.next;
    
    return dummy.next;
};

// Time Complexity: O(sz)
// Space Complexity: O(1)
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

function removeNthFromEnd(head: ListNode | null, n: number): ListNode | null {
    let dummy = new ListNode(0);
    dummy.next = head;
    let first: ListNode | null = dummy;
    let second: ListNode | null = dummy;
    
    // Move first pointer n+1 steps ahead
    for (let i = 0; i <= n; i++) {
        first = first.next;
    }
    
    // Move both pointers until first reaches the end
    while (first !== null) {
        first = first.next;
        second = second.next;
    }
    
    // Remove the nth node from the end
    second.next = second.next?.next ?? null;
    
    return dummy.next;
}

// Time Complexity: O(sz)
// Space Complexity: O(1)
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
     * @param Integer $n
     * @return ListNode
     */
    function removeNthFromEnd($head, $n) {
        $dummy = new ListNode(0);
        $dummy->next = $head;
        $first = $dummy;
        $second = $dummy;
        
        // Move first pointer n+1 steps ahead
        for ($i = 0; $i <= $n; $i++) {
            $first = $first->next;
        }
        
        // Move both pointers until first reaches the end
        while ($first !== null) {
            $first = $first->next;
            $second = $second->next;
        }
        
        // Remove the nth node from the end
        $second->next = $second->next->next;
        
        return $dummy->next;
    }
}

// Time Complexity: O(sz)
// Space Complexity: O(1)
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
    func removeNthFromEnd(_ head: ListNode?, _ n: Int) -> ListNode? {
        let dummy = ListNode(0)
        dummy.next = head
        var first: ListNode? = dummy
        var second: ListNode? = dummy
        
        // Move first pointer n+1 steps ahead
        for _ in 0...n {
            first = first?.next
        }
        
        // Move both pointers until first reaches the end
        while first != nil {
            first = first?.next
            second = second?.next
        }
        
        // Remove the nth node from the end
        second?.next = second?.next?.next
        
        return dummy.next
    }
}

// Time Complexity: O(sz)
// Space Complexity: O(1)
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
    fun removeNthFromEnd(head: ListNode?, n: Int): ListNode? {
        val dummy = ListNode(0)
        dummy.next = head
        var first: ListNode? = dummy
        var second: ListNode? = dummy
        
        // Move first pointer n+1 steps ahead
        for (i in 0..n) {
            first = first?.next
        }
        
        // Move both pointers until first reaches the end
        while (first != null) {
            first = first.next
            second = second?.next
        }
        
        // Remove the nth node from the end
        second?.next = second?.next?.next
        
        return dummy.next
    }
}

// Time Complexity: O(sz)
// Space Complexity: O(1)
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
  ListNode? removeNthFromEnd(ListNode? head, int n) {
    ListNode dummy = ListNode(0, head);
    ListNode? first = dummy;
    ListNode? second = dummy;
    
    // Move first pointer n+1 steps ahead
    for (int i = 0; i <= n; i++) {
      first = first?.next;
    }
    
    // Move both pointers until first reaches the end
    while (first != null) {
      first = first?.next;
      second = second?.next;
    }
    
    // Remove the nth node from the end
    second?.next = second?.next?.next;
    
    return dummy.next;
  }
}

// Time Complexity: O(sz)
// Space Complexity: O(1)
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
func removeNthFromEnd(head *ListNode, n int) *ListNode {
    dummy := &ListNode{0, head}
    first := dummy
    second := dummy
    
    // Move first pointer n+1 steps ahead
    for i := 0; i <= n; i++ {
        first = first.Next
    }
    
    // Move both pointers until first reaches the end
    for first != nil {
        first = first.Next
        second = second.Next
    }
    
    // Remove the nth node from the end
    second.Next = second.Next.Next
    
    return dummy.Next
}

// Time Complexity: O(sz)
// Space Complexity: O(1)
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
# @param {Integer} n
# @return {ListNode}
def remove_nth_from_end(head, n)
    dummy = ListNode.new(0)
    dummy.next = head
    first = dummy
    second = dummy
    
    # Move first pointer n+1 steps ahead
    (n+1).times do
        first = first.next
    end
    
    # Move both pointers until first reaches the end
    while first != nil
        first = first.next
        second = second.next
    end
    
    # Remove the nth node from the end
    second.next = second.next.next
    
    return dummy.next
end

# Time Complexity: O(sz)
# Space Complexity: O(1)
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
    def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
        val dummy = new ListNode(0)
        dummy.next = head
        var first: ListNode = dummy
        var second: ListNode = dummy
        
        // Move first pointer n+1 steps ahead
        for (_ <- 0 to n) {
            first = first.next
        }
        
        // Move both pointers until first reaches the end
        while (first != null) {
            first = first.next
            second = second.next
        }
        
        // Remove the nth node from the end
        second.next = second.next.next
        
        dummy.next
    }
}

// Time Complexity: O(sz)
// Space Complexity: O(1)
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
    pub fn remove_nth_from_end(head: Option<Box<ListNode>>, n: i32) -> Option<Box<ListNode>> {
        let mut dummy = Box::new(ListNode::new(0));
        dummy.next = head;
        let mut first = &mut dummy as *mut _;
        let mut second = &mut dummy;
        
        // Move first pointer n+1 steps ahead
        for _ in 0..=n {
            first = unsafe { &mut *first }.next.as_mut().unwrap();
        }
        
        // Move both pointers until first reaches the end
        while unsafe { &*first }.next.is_some() {
            first = unsafe { &mut *first }.next.as_mut().unwrap();
            second = second.next.as_mut().unwrap();
        }
        
        // Remove the nth node from the end
        second.next = second.next.as_mut().unwrap().next.take();
        
        dummy.next
    }
}

// Time Complexity: O(sz)
// Space Complexity: O(1)
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

(define/contract (remove-nth-from-end head n)
  (-> (or/c list-node? #f) exact-integer? (or/c list-node? #f))
  (define dummy (make-list-node 0))
  (set-list-node-next! dummy head)
  (define first dummy)
  (define second dummy)
  
  ; Move first pointer n+1 steps ahead
  (for ([i (in-range (+ n 1))])
    (set! first (list-node-next first)))
  
  ; Move both pointers until first reaches the end
  (while (list-node-next first)
    (set! first (list-node-next first))
    (set! second (list-node-next second)))
  
  ; Remove the nth node from the end
  (set-list-node-next! second (list-node-next (list-node-next second)))
  
  (list-node-next dummy)
)

; Time Complexity: O(sz)
; Space Complexity: O(1)
```

### Closing Statement

In today's coding interview, we successfully tackled the "Remove N-th Node From End of List" problem. We first discussed the brute force approach, where we computed the length of the linked list in one pass and then used a second pass to remove the n-th node from the end. This approach has a time complexity of \( O(sz) \) and a space complexity of \( O(1) \).

We then optimized our solution by implementing a two-pointer technique that achieves the task in a single pass. This improvement retains the \( O(sz) \) time complexity while maintaining a space complexity of \( O(1) \). We further translated this efficient solution across multiple programming languages such as C++, Java, Python, C, C#, JavaScript, PHP, Swift, Kotlin, Dart, Go, Ruby, Scala, Rust, Racket, Erlang, and Elixir.

This exercise not only demonstrated our understanding of linked list manipulations but also showcased our ability to optimize solutions and adapt them to different programming environments.

### Similar Questions

1. **Reverse a Linked List**:
   - Given the head of a singly linked list, reverse the list and return the reversed list.

2. **Merge Two Sorted Lists**:
   - Merge two sorted linked lists into one sorted linked list and return the merged list.

3. **Detect Cycle in a Linked List**:
   - Write an algorithm to detect if a linked list has a cycle in it. If a cycle is found, return the node where the cycle begins.

4. **Remove Duplicates from Sorted List**:
   - Given the head of a sorted linked list, delete all duplicates such that each element appears only once.

5. **Find Middle of Linked List**:
   - Given a non-empty, singly linked list with the head node, return the middle node of the linked list.

6. **Add Two Numbers**:
   - Given two non-empty linked lists representing two non-negative integers, add the two numbers and return it as a linked list.

7. **Flatten a Multilevel Doubly Linked List**:
   - You are given a doubly linked list, which contains nodes. These nodes might also be connected in a multilevel doubly linked list, where a child node is part of another doubly linked list. Flatten and merge all the levels of the doubly linked list into a single-level doubly linked list.

8. **Palindrome Linked List**:
   - Determine whether a given linked list is a palindrome.

I hope this session was insightful and enriching, and I look forward to our next coding challenge!