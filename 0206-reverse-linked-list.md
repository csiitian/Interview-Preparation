### Interviewer and Interviewee Discussion

#### Interviewer:
Let's start with a simple yet fundamental problem in linked lists. You are given the `head` of a singly linked list, and your task is to reverse this list and return the reversed list. 

How would you approach this problem?

#### Interviewee:
The problem is straightforward since reversing a linked list is a classic problem. There are two main approaches: iterative and recursive. But, let's start with the brute-force method to ensure we understand the basics.

#### Interviewer:
Good. Let's dive into the brute-force approach first. How would you implement it?

### Brute Force Approach

#### Interviewee:
In a brute-force solution, we can follow these steps:

1. Traverse the linked list to collect all node values in an array.
2. Reverse the array.
3. Create a new linked list using the reversed values.

This method isn't efficient in terms of space, but it ensures that we clearly understand the problem before optimizing. 

Here are the steps in pseudocode:

1. Initialize an empty array to store node values.
2. Traverse the linked list from `head` to the end, adding each node's value to the array.
3. Reverse the array.
4. Create a new linked list using the reversed array.

#### Interviewer:
Great, now let's talk about the time and space complexity of this brute-force approach.

#### Interviewee:
Sure.

- **Time Complexity**: O(n), where n is the number of nodes in the linked list because we traverse the entire list once.
- **Space Complexity**: O(n), for storing the node values in an array.

#### Interviewer:
That sounds good, but obviously, we need a more efficient approach given the constraints. Can you propose a more optimal solution?

### Optimized Approach

#### Interviewee:
Certainly. A more efficient way to reverse a linked list in terms of space complexity is to do it iteratively in-place. This involves changing the pointers of the nodes rather than using additional space.

Here's how it can be done iteratively:

1. Initialize three pointers: 
   - `prev` to `null` 
   - `current` to `head`
   - `next` to `null`
2. Traverse the linked list and at each step do the following:
   - Save the next node: `next = current.next`
   - Reverse the current node's pointer: `current.next = prev`
   - Move `prev` and `current` one step forward: `prev = current`, `current = next`
3. After the loop, `prev` will be the new head of the reversed list.

Here’s a visual representation of the pointer reassignments:

```
Initial state:
head -> 1 -> 2 -> 3 -> 4 -> 5 -> null

Iterate through the list:
Iteration 1:
prev = null, current = 1, next = 2
1.next changes from 2 to null
prev moves to 1, current moves to 2

Iteration 2:
prev = 1, current = 2, next = 3
2.next changes from 3 to 1
prev moves to 2, current moves to 3

...

Final state:
prev -> 5 -> 4 -> 3 -> 2 -> 1 -> null
```

Here’s the code to implement the iterative approach in Python:

```python
class ListNode:
    def __init__(self, val=0, next=None):
        self.val = val
        self.next = next

def reverseList(head: ListNode) -> ListNode:
    prev = None
    current = head

    while current:
        next_node = current.next
        current.next = prev
        prev = current
        current = next_node

    return prev
```

#### Interviewer:
Excellent! And what is the time and space complexity of this optimized approach?

#### Interviewee:
The optimized approach has:

- **Time Complexity**: O(n), where n is the number of nodes in the linked list since we still traverse all nodes once.
- **Space Complexity**: O(1), as we only use a constant amount of extra space for the pointers `prev`, `current`, and `next`.

#### Interviewer:
Great, would you also be able to implement a recursive solution?

#### Interviewee:
Sure, here’s how you can do it recursively. The idea is to keep diving to the end of the list and then start reversing the pointers on the way back.

Here’s the code for a recursive approach:

```python
def reverseListRecursive(head: ListNode) -> ListNode:
    if not head or not head.next:
        return head

    new_head = reverseListRecursive(head.next)
    head.next.next = head
    head.next = None

    return new_head
```

In this recursive method:

- We reach the end of the list first.
- Then on unwinding the recursive calls, we start reversing the pointers.

#### Interviewer:
Very good. What are the time and space complexities for the recursive solution?

#### Interviewee:
- **Time Complexity**: O(n), as we need to traverse all nodes.
- **Space Complexity**: O(n), due to the recursion stack depth which goes as deep as the number of nodes in the list.

#### Interviewer:
Excellent! You've covered both iterative and recursive approaches with clarity. Thank you for explaining it so well.

#### Interviewee:
Thank you! It's always great to work through these fundamental problems.
Sure! I will provide the iterative solution in each of the specified languages, follow with brief explanations on the time and space complexity.

### C++ Implementation

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
    ListNode* reverseList(ListNode* head) {
        ListNode* prev = nullptr;
        ListNode* current = head;
        
        while (current != nullptr) {
            ListNode* next_node = current->next;
            current->next = prev;
            prev = current;
            current = next_node;
        }

        return prev;
    }
};

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Java Implementation

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
    public ListNode reverseList(ListNode head) {
        ListNode prev = null;
        ListNode current = head;
        
        while (current != null) {
            ListNode next_node = current.next;
            current.next = prev;
            prev = current;
            current = next_node;
        }

        return prev;
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Python Implementation

```python
# Definition for singly-linked list.
# class ListNode(object):
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next

class Solution(object):
    def reverseList(self, head):
        """
        :type head: ListNode
        :rtype: ListNode
        """
        prev = None
        current = head

        while current:
            next_node = current.next
            current.next = prev
            prev = current
            current = next_node

        return prev

# Time Complexity: O(n)
# Space Complexity: O(1)
```

### Python 3 Implementation

```python
# Definition for singly-linked list.
# class ListNode:
#     def __init__(self, val=0, next=None):
#         self.val = val
#         self.next = next

class Solution:
    def reverseList(self, head: Optional[ListNode]) -> Optional[ListNode]:
        prev = None
        current = head

        while current:
            next_node = current.next
            current.next = prev
            prev = current
            current = next_node

        return prev

# Time Complexity: O(n)
# Space Complexity: O(1)
```

### C Implementation

```c
/**
 * Definition for singly-linked list.
 * struct ListNode {
 *     int val;
 *     struct ListNode *next;
 * };
 */
struct ListNode* reverseList(struct ListNode* head) {
    struct ListNode* prev = NULL;
    struct ListNode* current = head;
    
    while (current != NULL) {
        struct ListNode* next_node = current->next;
        current->next = prev;
        prev = current;
        current = next_node;
    }

    return prev;
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### C# Implementation

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
    public ListNode ReverseList(ListNode head) {
        ListNode prev = null;
        ListNode current = head;

        while (current != null) {
            ListNode next_node = current.next;
            current.next = prev;
            prev = current;
            current = next_node;
        }

        return prev;
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### JavaScript Implementation

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
var reverseList = function(head) {
    let prev = null;
    let current = head;

    while (current !== null) {
        let next_node = current.next;
        current.next = prev;
        prev = current;
        current = next_node;
    }

    return prev;
};

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### TypeScript Implementation

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

function reverseList(head: ListNode | null): ListNode | null {
    let prev: ListNode | null = null;
    let current: ListNode | null = head;

    while (current !== null) {
        let next_node: ListNode | null = current.next;
        current.next = prev;
        prev = current;
        current = next_node;
    }

    return prev;
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### PHP Implementation

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
    function reverseList($head) {
        $prev = null;
        $current = $head;

        while ($current != null) {
            $next_node = $current->next;
            $current->next = $prev;
            $prev = $current;
            $current = $next_node;
        }

        return $prev;
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Swift Implementation

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
    func reverseList(_ head: ListNode?) -> ListNode? {
        var prev: ListNode? = nil
        var current = head
        
        while current != nil {
            let next_node = current?.next
            current?.next = prev
            prev = current
            current = next_node
        }

        return prev
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Kotlin Implementation

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
    fun reverseList(head: ListNode?): ListNode? {
        var prev: ListNode? = null
        var current = head
        
        while (current != null) {
            val next_node = current.next
            current.next = prev
            prev = current
            current = next_node
        }

        return prev
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Dart Implementation

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
  ListNode? reverseList(ListNode? head) {
    ListNode? prev = null;
    ListNode? current = head;
    
    while (current != null) {
      ListNode? next_node = current.next;
      current.next = prev;
      prev = current;
      current = next_node;
    }

    return prev;
  }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Go Implementation

```go
/**
 * Definition for singly-linked list.
 * type ListNode struct {
 *     Val int
 *     Next *ListNode
 * }
 */
func reverseList(head *ListNode) *ListNode {
    var prev *ListNode
    current := head
    
    for current != nil {
        nextNode := current.Next
        current.Next = prev
        prev = current
        current = nextNode
    }
    
    return prev
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Ruby Implementation

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
def reverse_list(head)
    prev = nil
    current = head
    
    while current != nil
        next_node = current.next
        current.next = prev
        prev = current
        current = next_node
    end
    
    prev
end

# Time Complexity: O(n)
# Space Complexity: O(1)
```

### Scala Implementation

```scala
/**
 * Definition for singly-linked list.
 * class ListNode(_x: Int = 0, _next: ListNode = null) {
 *   var next: ListNode = _next
 *   var x: Int = _x
 * }
 */
object Solution {
    def reverseList(head: ListNode): ListNode = {
        var prev: ListNode = null
        var current = head
        
        while (current != null) {
            val next_node = current.next
            current.next = prev
            prev = current
            current = next_node
        }
        
        prev
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Rust Implementation

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
    pub fn reverse_list(head: Option<Box<ListNode>>) -> Option<Box<ListNode>> {
        let mut prev: Option<Box<ListNode>> = None;
        let mut current = head;
        
        while let Some(mut current_node) = current {
            current = current_node.next.take();
            current_node.next = prev;
            prev = Some(current_node);
        }
        
        prev
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Racket Implementation

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

(define/contract (reverse-list head)
  (-> (or/c list-node? #f) (or/c list-node? #f))
  (let loop ([prev #f] [current head])
    (if (null? current)
        prev
        (let ([next (list-node-next current)])
          (set-list-node-next! current prev)
          (loop current next))))

; Time Complexity: O(n)
; Space Complexity: O(1)
```

### Erlang Implementation

```erlang
%% Definition for singly-linked list.
%%
%% -record(list_node, {val = 0 :: integer(),
%%                     next = null :: 'null' | #list_node{}}).

-spec reverse_list(Head :: #list_node{} | null) -> #list_node{} | null.
reverse_list(Head) ->
  loop(Head, null).

loop(Current, Prev) ->
  case Current of
    null -> Prev;
    #list_node{val=Val, next=NextNode} ->
      Reversed = #list_node{val=Val, next=Prev},
      loop(NextNode, Reversed)
  end.

% Time Complexity: O(n)
% Space Complexity: O(1)
```

### Elixir Implementation

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
  @spec reverse_list(head :: ListNode.t | nil) :: ListNode.t | nil
  def reverse_list(head) do
    loop(nil, head)
  end

  defp loop(prev, nil), do: prev
  defp loop(prev, %ListNode{val: val, next: next_node}) do
    new_node = %ListNode{val: val, next: prev}
    loop(new_node, next_node)
  end
end

# Time Complexity: O(n)
# Space Complexity: O(1)
```

This covers providing the iterative approach to reverse a singly linked list in multiple programming languages with their respective time and space complexities.


### Closing Statement

In this discussion, we've explored various ways to reverse a singly linked list, starting from a brute-force approach and optimizing towards an efficient iterative solution with O(n) time complexity and O(1) space complexity. We also examined a recursive solution that achieves the same time complexity but comes with a higher space complexity due to the recursion stack. The implementation details were provided across multiple programming languages, making it evident that mastering fundamental algorithms and data structures transcends language barriers and forms the basis for solving such problems efficiently.

Understanding and practicing these fundamental operations on linked lists not only strengthens your problem-solving skills but also prepares you for more complex challenges that you might encounter in your programming journey.

### Similar Questions

1. **Palindrome Linked List**
   - *Question*: Given the head of a singly linked list, determine if it is a palindrome.
   - *Concepts*: Linked list, two-pointer technique, reversing a linked list.

2. **Merge Two Sorted Lists**
   - *Question*: Given the heads of two sorted linked lists, merge them into one sorted linked list.
   - *Concepts*: Linked list, merging, pointers.

3. **Remove Nth Node From End of List**
   - *Question*: Given the head of a linked list, remove the nth node from the end of the list and return its head.
   - *Concepts*: Linked list, two-pointer technique.

4. **Linked List Cycle**
   - *Question*: Given the head of a linked list, determine if the linked list has a cycle.
   - *Concepts*: Linked list, two-pointer technique (Floyd’s Tortoise and Hare).

5. **Intersection of Two Linked Lists**
   - *Question*: Given the heads of two singly linked lists, find the node at which the two lists intersect.
   - *Concepts*: Linked list, pointers.

6. **Rotate List**
   - *Question*: Given the head of a linked list and an integer k, rotate the list to the right by k places.
   - *Concepts*: Linked list manipulation.

### Wrapping Up

These problems will challenge your understanding and manipulation of linked lists and deepen your algorithmic thinking. Mastery of such questions will certainly enhance your ability to solve complex data structure problems confidently. Good luck, and happy coding!