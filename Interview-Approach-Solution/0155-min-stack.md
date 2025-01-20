### Interviewer and Interviewee Discussion

**Interviewer:** Today, I'd like to discuss a problem that involves designing a stack with additional functionality. The stack you design should support push, pop, top, and an additional operation to retrieve the minimum element in constant time. Are you familiar with stacks and their basic operations?

**Interviewee:** Yes, I am familiar with stacks. I understand that stacks follow the Last In, First Out (LIFO) principle, and the basic operations are push (to add an element), pop (to remove the top element), and top (to retrieve the top element without removing it).

**Interviewer:** Great. In addition to these basic operations, your stack should also be able to retrieve the minimum element in O(1) time. Let me describe the class you need to implement:
- `MinStack()` initializes the stack object.
- `void push(int val)` pushes the element `val` onto the stack.
- `void pop()` removes the element on the top of the stack.
- `int top()` gets the top element of the stack.
- `int getMin()` retrieves the minimum element in the stack.

Feel free to start by discussing a brute-force approach.

### Initial Thoughts on Brute Force Approach

**Interviewee:** Let's start with the brute-force approach. In a naïve implementation, I could maintain a normal stack for basic operations and an additional array or list to store the minimum elements seen so far. However, the main challenge is to ensure that `getMin()` allows retrieving the minimum element in O(1) time.

In a brute-force:
1. **Push Operation:**
   - Push the element onto the stack.
   - Traverse the stack to update the minimum element (costly).

2. **Pop Operation:**
   - Pop the element from the stack.
   - Traverse the stack to update the minimum element (costly if minimum element is popped).

3. **Top Operation:**
   - Simply return the element at the top of the stack (O(1)).

4. **GetMin Operation:**
   - Return the precomputed minimum element (can be costly during updates in a brute-force approach).

**Interviewer:** That sounds good as a start. This brute-force method is inefficient due to the costly traversals for getMin. Can you explain the time and space complexity for this brute-force approach?

**Interviewee:** Certainly:
- **Push Operation:** O(n) in the worst case (if updating minimum requires traversing the whole stack).
- **Pop Operation:** O(n) in the worst case (if the popped element is the minimum and we need to traverse to find the new minimum).
- **Top Operation:** O(1).
- **GetMin Operation:** O(1) for retrieval, but the worst-case cost is amortized due to the costly traversals during push and pop.

**Space Complexity:** O(n) for storing elements plus potentially another O(n) for maintaining the list of minimum elements.

**Interviewer:** That's correct. Now, can you think of a way to optimize the solution?

### Optimized Approach

**Interviewee:** To optimize the solution, we can use an auxiliary stack to keep track of the minimum values. Let's call the main stack `st` and the auxiliary stack `min_st`. Here's how we can use two stacks to achieve O(1) time complexity for all operations:

1. **Push Operation:**
   - Push the element onto the `st`.
   - If `min_st` is empty or the pushed element is smaller than or equal to the top of `min_st`, push it onto `min_st`.

2. **Pop Operation:**
   - Pop the element from `st`.
   - If the popped element is equal to the top of `min_st`, pop from `min_st` as well.

3. **Top Operation:**
   - Simply return the top element from `st`.

4. **GetMin Operation:**
   - Return the top element from `min_st`.

This helps maintain the minimum element efficiently.

#### Time and Space Complexity
- **Time Complexity:**
  - All operations (push, pop, top, getMin) are O(1).

- **Space Complexity:**
  - O(n) for storing elements in both `st` and `min_st`.

**Interviewer:** That sounds efficient. Can you explain this using a diagram?

**Interviewee:** Certainly. Let's push a sequence of elements to show how it works.

1. Push `-2`:

```
st: [-2]
min_st: [-2]
```

2. Push `0`:

```
st: [-2, 0]
min_st: [-2]
```

3. Push `-3`:

```
st: [-2, 0, -3]
min_st: [-2, -3]
```

4. GetMin:

```
min_st: Top is -3
```

5. Pop (removes `-3`):

```
st: [-2, 0]
min_st: [-2]
```

6. Top:

```
st: Top is 0
```

7. GetMin again:

```
min_st: Top is -2
```

**Interviewer:** Excellent. You've optimally designed the MinStack with an efficient approach and demonstrated it clearly. Well done.
### C++

```cpp
class MinStack {
public:
    MinStack() {
        
    }
    
    void push(int val) {
        st.push(val);
        if (min_st.empty() || val <= min_st.top()) {
            min_st.push(val);
        }
    }
    
    void pop() {
        if (st.top() == min_st.top()) {
            min_st.pop();
        }
        st.pop();
    }
    
    int top() {
        return st.top();
    }
    
    int getMin() {
        return min_st.top();
    }

private:
    std::stack<int> st;
    std::stack<int> min_st;
};

/**
 * Your MinStack object will be instantiated and called as such:
 * MinStack* obj = new MinStack();
 * obj->push(val);
 * obj->pop();
 * int param_3 = obj->top();
 * int param_4 = obj->getMin();
 */
```

### Java

```java
class MinStack {

    private Stack<Integer> stack;
    private Stack<Integer> minStack;

    public MinStack() {
        stack = new Stack<>();
        minStack = new Stack<>();
    }
    
    public void push(int val) {
        stack.push(val);
        if (minStack.isEmpty() || val <= minStack.peek()) {
            minStack.push(val);
        }
    }
    
    public void pop() {
        if (stack.peek().equals(minStack.peek())) {
            minStack.pop();
        }
        stack.pop();
    }
    
    public int top() {
        return stack.peek();
    }
    
    public int getMin() {
        return minStack.peek();
    }
}

/**
 * Your MinStack object will be instantiated and called as such:
 * MinStack obj = new MinStack();
 * obj.push(val);
 * obj.pop();
 * int param_3 = obj.top();
 * int param_4 = obj.getMin();
 */
```

### Python

```python
class MinStack(object):

    def __init__(self):
        self.stack = []
        self.min_stack = []

    def push(self, val):
        """
        :type val: int
        :rtype: None
        """
        self.stack.append(val)
        if not self.min_stack or val <= self.min_stack[-1]:
            self.min_stack.append(val)

    def pop(self):
        """
        :rtype: None
        """
        if self.stack[-1] == self.min_stack[-1]:
            self.min_stack.pop()
        self.stack.pop()

    def top(self):
        """
        :rtype: int
        """
        return self.stack[-1]

    def getMin(self):
        """
        :rtype: int
        """
        return self.min_stack[-1]

# Your MinStack object will be instantiated and called as such:
# obj = MinStack()
# obj.push(val)
# obj.pop()
# param_3 = obj.top()
# param_4 = obj.getMin()
```

### Python3

```python
class MinStack:

    def __init__(self):
        self.stack = []
        self.min_stack = []

    def push(self, val: int) -> None:
        self.stack.append(val)
        if not self.min_stack or val <= self.min_stack[-1]:
            self.min_stack.append(val)

    def pop(self) -> None:
        if self.stack[-1] == self.min_stack[-1]:
            self.min_stack.pop()
        self.stack.pop()

    def top(self) -> int:
        return self.stack[-1]

    def getMin(self) -> int:
        return self.min_stack[-1]

# Your MinStack object will be instantiated and called as such:
# obj = MinStack()
# obj.push(val)
# obj.pop()
# param_3 = obj.top()
# param_4 = obj.getMin()
```

### C

```c
typedef struct {
    int* stack;
    int* min_stack;
    int top_idx;
    int min_idx;
} MinStack;

MinStack* minStackCreate() {
    MinStack* obj = (MinStack*)malloc(sizeof(MinStack));
    obj->stack = (int*)malloc(sizeof(int) * 30000);
    obj->min_stack = (int*)malloc(sizeof(int) * 30000);
    obj->top_idx = -1;
    obj->min_idx = -1;
    return obj;
}

void minStackPush(MinStack* obj, int val) {
    obj->stack[++obj->top_idx] = val;
    if (obj->min_idx == -1 || val <= obj->min_stack[obj->min_idx]) {
        obj->min_stack[++obj->min_idx] = val;
    }
}

void minStackPop(MinStack* obj) {
    if (obj->stack[obj->top_idx] == obj->min_stack[obj->min_idx]) {
        obj->min_idx--;
    }
    obj->top_idx--;
}

int minStackTop(MinStack* obj) {
    return obj->stack[obj->top_idx];
}

int minStackGetMin(MinStack* obj) {
    return obj->min_stack[obj->min_idx];
}

void minStackFree(MinStack* obj) {
    free(obj->stack);
    free(obj->min_stack);
    free(obj);
}

/**
 * Your MinStack struct will be instantiated and called as such:
 * MinStack* obj = minStackCreate();
 * minStackPush(obj, val);
 * minStackPop(obj);
 * int param_3 = minStackTop(obj);
 * int param_4 = minStackGetMin(obj);
 * minStackFree(obj);
 */
```

### C#

```csharp
public class MinStack {

    private Stack<int> stack;
    private Stack<int> minStack;

    public MinStack() {
        stack = new Stack<int>();
        minStack = new Stack<int>();
    }
    
    public void Push(int val) {
        stack.Push(val);
        if (minStack.Count == 0 || val <= minStack.Peek()) {
            minStack.Push(val);
        }
    }
    
    public void Pop() {
        if (stack.Peek() == minStack.Peek()) {
            minStack.Pop();
        }
        stack.Pop();
    }
    
    public int Top() {
        return stack.Peek();
    }
    
    public int GetMin() {
        return minStack.Peek();
    }
}

/**
 * Your MinStack object will be instantiated and called as such:
 * MinStack obj = new MinStack();
 * obj.Push(val);
 * obj.Pop();
 * int param_3 = obj.Top();
 * int param_4 = obj.GetMin();
 */
```

### JavaScript

```javascript
var MinStack = function() {
    this.stack = [];
    this.minStack = [];
};

/** 
 * @param {number} val
 * @return {void}
 */
MinStack.prototype.push = function(val) {
    this.stack.push(val);
    if (this.minStack.length === 0 || val <= this.minStack[this.minStack.length - 1]) {
        this.minStack.push(val);
    }
};

/**
 * @return {void}
 */
MinStack.prototype.pop = function() {
    if (this.stack[this.stack.length - 1] === this.minStack[this.minStack.length - 1]) {
        this.minStack.pop();
    }
    this.stack.pop();
};

/**
 * @return {number}
 */
MinStack.prototype.top = function() {
    return this.stack[this.stack.length - 1];
};

/**
 * @return {number}
 */
MinStack.prototype.getMin = function() {
    return this.minStack[this.minStack.length - 1];
};

/** 
 * Your MinStack object will be instantiated and called as such:
 * var obj = new MinStack()
 * obj.push(val)
 * obj.pop()
 * var param_3 = obj.top()
 * var param_4 = obj.getMin()
 */
```

### TypeScript

```typescript
class MinStack {
    private stack: number[];
    private minStack: number[];

    constructor() {
        this.stack = [];
        this.minStack = [];
    }

    push(val: number): void {
        this.stack.push(val);
        if (this.minStack.length === 0 || val <= this.minStack[this.minStack.length - 1]) {
            this.minStack.push(val);
        }
    }

    pop(): void {
        if (this.stack[this.stack.length - 1] === this.minStack[this.minStack.length - 1]) {
            this.minStack.pop();
        }
        this.stack.pop();
    }

    top(): number {
        return this.stack[this.stack.length - 1];
    }

    getMin(): number {
        return this.minStack[this.minStack.length - 1];
    }
}

/**
 * Your MinStack object will be instantiated and called as such:
 * var obj = new MinStack()
 * obj.push(val)
 * obj.pop()
 * var param_3 = obj.top()
 * var param_4 = obj.getMin()
 */
```

### PHP

```php
class MinStack {
    private $stack;
    private $minStack;

    public function __construct() {
        $this->stack = [];
        $this->minStack = [];
    }

    public function push($val) {
        array_push($this->stack, $val);
        if (empty($this->minStack) || $val <= end($this->minStack)) {
            array_push($this->minStack, $val);
        }
    }

    public function pop() {
        if (end($this->stack) == end($this->minStack)) {
            array_pop($this->minStack);
        }
        array_pop($this->stack);
    }

    public function top() {
        return end($this->stack);
    }

    public function getMin() {
        return end($this->minStack);
    }
}

/**
 * Your MinStack object will be instantiated and called as such:
 * $obj = MinStack();
 * $obj->push($val);
 * $obj->pop();
 * $ret_3 = $obj->top();
 * $ret_4 = $obj->getMin();
 */
```

### Swift

```swift
class MinStack {

    private var stack: [Int]
    private var minStack: [Int]

    init() {
        stack = [Int]()
        minStack = [Int]()
    }

    func push(_ val: Int) {
        stack.append(val)
        if minStack.isEmpty || val <= minStack.last! {
            minStack.append(val)
        }
    }

    func pop() {
        if stack.last == minStack.last {
            minStack.removeLast()
        }
        stack.removeLast()
    }

    func top() -> Int {
        return stack.last!
    }

    func getMin() -> Int {
        return minStack.last!
    }
}

/**
 * Your MinStack object will be instantiated and called as such:
 * let obj = MinStack()
 * obj.push(val)
 * obj.pop()
 * let ret_3: Int = obj.top()
 * let ret_4: Int = obj.getMin()
 */
```

### Kotlin

```kotlin
class MinStack() {

    private val stack = mutableListOf<Int>()
    private val minStack = mutableListOf<Int>()

    fun push(`val`: Int) {
        stack.add(`val`)
        if (minStack.isEmpty() || `val` <= minStack.last()) {
            minStack.add(`val`)
        }
    }

    fun pop() {
        if (stack.last() == minStack.last()) {
            minStack.removeAt(minStack.size - 1)
        }
        stack.removeAt(stack.size - 1)
    }

    fun top(): Int {
        return stack.last()
    }

    fun getMin(): Int {
        return minStack.last()
    }
}

/**
 * Your MinStack object will be instantiated and called as such:
 * var obj = MinStack()
 * obj.push(`val`)
 * obj.pop()
 * var param_3 = obj.top()
 * var param_4 = obj.getMin()
 */
```

### Dart

```dart
class MinStack {

  List<int> stack;
  List<int> minStack;

  MinStack() {
    stack = [];
    minStack = [];
  }
  
  void push(int val) {
    stack.add(val);
    if (minStack.isEmpty || val <= minStack.last) {
      minStack.add(val);
    }
  }
  
  void pop() {
    if (stack.last == minStack.last) {
      minStack.removeLast();
    }
    stack.removeLast();
  }
  
  int top() {
    return stack.last;
  }
  
  int getMin() {
    return minStack.last;
  }
}

/**
 * Your MinStack object will be instantiated and called as such:
 * MinStack obj = MinStack();
 * obj.push(val);
 * obj.pop();
 * int param3 = obj.top();
 * int param4 = obj.getMin();
 */
```

### Go

```go
type MinStack struct {
    stack    []int
    minStack []int
}

func Constructor() MinStack {
    return MinStack{
        stack:    []int{},
        minStack: []int{},
    }
}

func (this *MinStack) Push(val int) {
    this.stack = append(this.stack, val)
    if len(this.minStack) == 0 || val <= this.minStack[len(this.minStack)-1] {
        this.minStack = append(this.minStack, val)
    }
}

func (this *MinStack) Pop() {
    if this.stack[len(this.stack)-1] == this.minStack[len(this.minStack)-1] {
        this.minStack = this.minStack[:len(this.minStack)-1]
    }
    this.stack = this.stack[:len(this.stack)-1]
}

func (this *MinStack) Top() int {
    return this.stack[len(this.stack)-1]
}

func (this *MinStack) GetMin() int {
    return this.minStack[len(this.minStack)-1]
}

/**
 * Your MinStack object will be instantiated and called as such:
 * obj := Constructor();
 * obj.Push(val);
 * obj.Pop();
 * param_3 := obj.Top();
 * param_4 := obj.GetMin();
 */
```

### Ruby

```ruby
class MinStack
    def initialize()
        @stack = []
        @min_stack = []
    end

    def push(val)
        @stack.push(val)
        if @min_stack.empty? || val <= @min_stack.last
            @min_stack.push(val)
        end
    end

    def pop()
        if @stack.last == @min_stack.last
            @min_stack.pop
        end
        @stack.pop
    end

    def top()
        @stack.last
    end

    def get_min()
        @min_stack.last
    end
end

# Your MinStack object will be instantiated and called as such:
# obj = MinStack.new()
# obj.push(val)
# obj.pop()
# param_3 = obj.top()
# param_4 = obj.get_min()
```

### Scala

```scala
class MinStack() {

    private val stack = scala.collection.mutable.Stack[Int]()
    private val minStack = scala.collection.mutable.Stack[Int]()

    def push(`val`: Int): Unit = {
        stack.push(`val`)
        if (minStack.isEmpty || `val` <= minStack.head) {
            minStack.push(`val`)
        }
    }

    def pop(): Unit = {
        if (stack.head == minStack.head) {
            minStack.pop()
        }
        stack.pop()
    }

    def top(): Int = {
        stack.head
    }

    def getMin(): Int = {
        minStack.head
    }
}

/**
 * Your MinStack object will be instantiated and called as such:
 * val obj = new MinStack()
 * obj.push(`val`)
 * obj.pop()
 * val param_3 = obj.top()
 * val param_4 = obj.getMin()
 */
```

### Rust

```rust
struct MinStack {
    stack: Vec<i32>,
    min_stack: Vec<i32>,
}

impl MinStack {

    fn new() -> Self {
        MinStack {
            stack: Vec::new(),
            min_stack: Vec::new(),
        }
    }
    
    fn push(&mut self, val: i32) {
        self.stack.push(val);
        if self.min_stack.is_empty() || val <= *self.min_stack.last().unwrap() {
            self.min_stack.push(val);
        }
    }
    
    fn pop(&mut self) {
        if let Some(top) = self.stack.pop() {
            if top == *self.min_stack.last().unwrap() {
                self.min_stack.pop();
            }
        }
    }
    
    fn top(&self) -> i32 {
        *self.stack.last().unwrap()
    }
    
    fn get_min(&self) -> i32 {
        *self.min_stack.last().unwrap()
    }
}

/**
 * Your MinStack object will be instantiated and called as such:
 * let obj = MinStack::new();
 * obj.push(val);
 * obj.pop();
 * let ret_3: i32 = obj.top();
 * let ret_4: i32 = obj.get_min();
 */
```

### Racket

```racket
(define min-stack%
  (class object%
    (super-new)
    
    (init-field stack min-stack)
    
    ; push : exact-integer? -> void?
    (define/public (push val)
      (send stack push val)
      (if (or (send min-stack empty?)
              (<= val (send min-stack top)))
          (send min-stack push val)))
    
    ; pop : -> void?
    (define/public (pop)
      (if (= (send stack top) (send min-stack top))
          (send min-stack pop))
      (send stack pop))
    
    ; top : -> exact-integer?
    (define/public (top)
      (send stack top))
    
    ; get-min : -> exact-integer?
    (define/public (get-min)
      (send min-stack top))))

;; Your min-stack% object will be instantiated and called as such:
;; (define obj (new min-stack%))
;; (send obj push val)
;; (send obj pop)
;; (define param_3 (send obj top))
;; (define param_4 (send obj get-min))
```


### Closing Statement

**Interviewer:** Excellent work! You've successfully implemented a MinStack class in multiple programming languages with optimized push, pop, top, and getMin operations. Your understanding of how to maintain the minimum element efficiently using auxiliary data structures shows a strong grasp of fundamental data structures and algorithms. Implementing these solutions in different languages demonstrates your versatility and adaptability to various programming environments. 

Do you have any questions or thoughts about the implementation?

**Interviewee:** Thank you! This was a great exercise in understanding both stack operations and optimization. It was interesting to see how different languages handle similar data structures.

**Interviewer:** I'm glad you enjoyed it. You should feel confident in handling such problems in future interviews or coding challenges. Great job!

### Similar Questions for Practice

Here are some similar questions you might find useful for further practice:

1. **Design a Queue that supports push, pop, front, and retrieving the maximum element in constant time.**
    - This question swaps the stack with a queue and focuses on retrieving the maximum element instead of the minimum.

2. **Implement a Stack that supports additional operations for O(1) time complexity: getMax() to retrieve the maximum element in the stack.**
    - This is a direct variation which asks for the maximum instead of minimum.

3. **Design a data structure that supports insert, delete, search, and getRandom in constant time.**
    - This question focuses on dynamically manipulating a collection of elements while still allowing O(1) complexity for random access.

4. **Implement a Data Structure that supports push, pop, top, getMin, and getMax in constant time.**
    - Combining both getMin and getMax functions may require careful thinking about the auxiliary stack or data structure used.

5. **Implement a Queue using Stacks.**
    - It's a classic problem that tests your understanding of how you can use stack operations to simulate a queue's behavior.

6. **Design a circular buffer or a circular queue.**
    - This problem involves implementing a cyclic data structure with efficient access operations.

Practicing these questions will further strengthen your understanding and problem-solving skills in data structures and algorithm optimization. Good luck!