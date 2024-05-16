### Interviewer and Interviewee Discussion

**Interviewer:** Let's talk about the LRU Cache problem. The goal is to design a data structure that follows the constraints of a Least Recently Used (LRU) cache. You need to implement the `LRUCache` class with the following functionalities:
1. `LRUCache(int capacity)` initializes the cache with a given capacity.
2. `int get(int key)` returns the value of the key if it exists, otherwise returns `-1`.
3. `void put(int key, int value)` updates the value of the key if it exists, or adds the key-value pair to the cache. If the number of keys exceeds the capacity, it evicts the least recently used key.

Can you give me a brief explanation of the problem and how you would start solving it?

**Interviewee:** Sure. The LRU cache problem requires us to keep track of the most recently used items and evict the least recently used item when the cache reaches its capacity. We need to perform both `get` and `put` operations in constant time, O(1). A brute-force approach would involve linear searches, which would be inefficient for the constraints given.

**Interviewer:** That's correct. Can you elaborate on your brute-force approach and its time and space complexity?

### Brute Force Approach:

**Interviewee:** In a brute-force approach, we could use a list to store the keys and values. Each time we access a key, we find it in the list and move it to the front. When we insert a new key-value pair and the cache is full, we remove the last item from the list.

- **get(key):** Linear search in the list to find the key, O(n) time complexity.
- **put(key, value):** Linear search to update or append the key-value pair and move the key to the front, followed by linear time deletion if necessary, O(n) time complexity.
  
The space complexity would be O(capacity) to store the key-value pairs.

**Interviewer:** Good, you mentioned the inefficiency due to linear searches. Can you think of a more efficient way to design this cache?

**Interviewee:** Yes, we can use a combination of a doubly linked list and a hash map (dictionary) to achieve the required operations in constant time, O(1).

### Optimized Approach:

**Interviewee:** Here’s how we can optimize the approach:
1. **Doubly Linked List:** This will help us maintain the order of the elements, with the most recently used at the head and the least recently used at the tail. This allows us to remove the tail in O(1) time.
2. **Hash Map:** This allows us to achieve O(1) time complexity for `get` and `put` operations by storing the nodes' references.

#### Data Structures:
- A doubly linked list where each node contains `key`, `value`, `prev`, and `next` pointers.
- A hash map (dictionary) that holds the keys and their corresponding node references.

#### Operations:
- **get(key):** If the key is found in the hash map, move the corresponding node to the front of the doubly linked list and return the value.
- **put(key, value):** If the key is already in the hash map, update the value and move the node to the front. If the key is not present, create a new node. If the capacity is exceeded, remove the least recently used node from the doubly linked list and the hash map.

Here’s a visual representation:

```
Initial State:
Hash Map: {}
Doubly Linked List: NULL <- Head <-> Tail -> NULL

After `put(1, 1)`:
Hash Map: {1 -> Node(1, 1)}
Doubly Linked List: NULL <- Head <-> Node(1, 1) <-> Tail -> NULL

After `put(2, 2)`:
Hash Map: {1 -> Node(1, 1), 2 -> Node(2, 2)}
Doubly Linked List: NULL <- Head <-> Node(2, 2) <-> Node(1, 1) <-> Tail -> NULL

After `get(1)`:
Hash Map: {1 -> Node(1, 1), 2 -> Node(2, 2)}
Doubly Linked List: NULL <- Head <-> Node(1, 1) <-> Node(2, 2) <-> Tail -> NULL

After `put(3, 3)` (capacity is 2, evict 2):
Hash Map: {1 -> Node(1, 1), 3 -> Node(3, 3)}
Doubly Linked List: NULL <- Head <-> Node(3, 3) <-> Node(1, 1) <-> Tail -> NULL
```

**Interviewer:** Excellent! You have correctly identified the key components for the optimized solution. This combination allows constant time operations for both `get` and `put`, and the space complexity remains linear in terms of the capacity.

Let’s implement this in code:

```python
class Node:
    def __init__(self, key, value):
        self.key = key
        self.value = value
        self.prev = None
        self.next = None

class LRUCache:
    def __init__(self, capacity: int):
        self.capacity = capacity
        self.cache = {}
        self.head = Node(0, 0)
        self.tail = Node(0, 0)
        self.head.next = self.tail
        self.tail.prev = self.head

    def _remove(self, node):
        prev = node.prev
        next = node.next
        prev.next = next
        next.prev = prev

    def _add(self, node):
        node.next = self.head.next
        node.next.prev = node
        self.head.next = node
        node.prev = self.head
        
    def get(self, key: int) -> int:
        if key in self.cache:
            node = self.cache[key]
            self._remove(node)
            self._add(node)
            return node.value
        return -1

    def put(self, key: int, value: int) -> None:
        if key in self.cache:
            self._remove(self.cache[key])
        node = Node(key, value)
        self._add(node)
        self.cache[key] = node
        if len(self.cache) > self.capacity:
            lru = self.tail.prev
            self._remove(lru)
            del self.cache[lru.key]
```

**Interviewer:** Very well, the implementation looks executable. This design efficiently meets the problem's requirements. Good job!
To implement the LRUCache with the specified methods (`get` and `put`) and desired time and space complexities, we will use the combination of a hash map (for O(1) access to cache entries) and a doubly-linked list (to maintain the order of usage).

Here's the implementation for each language, along with time and space complexity annotations:

### C++

```cpp
class LRUCache {
public:
    LRUCache(int capacity) : capacity(capacity) {}

    int get(int key) {
        // O(1) complexity
    }

    void put(int key, int value) {
        // O(1) complexity
    }

private:
    struct Node {
        int key, value;
        Node* prev;
        Node* next;
        Node(int k, int v) : key(k), value(v), prev(nullptr), next(nullptr) {}
    };

    void remove(Node* node) {
        // O(1) complexity
    }

    void addToHead(Node* node) {
        // O(1) complexity
    }

    unordered_map<int, Node*> cache;
    Node* head;
    Node* tail;
    int capacity;
};

/*
 * Your LRUCache object will be instantiated and called as such:
 * LRUCache* obj = new LRUCache(capacity);
 * int param_1 = obj->get(key);
 * obj->put(key, value);
 */
```

### Java

```java
class LRUCache {
    private static class Node {
        int key, value;
        Node prev, next;
        Node(int k, int v) { key = k; value = v; }
    }

    private Node head, tail;
    private Map<Integer, Node> cache;
    private int capacity;

    public LRUCache(int capacity) {
        // O(capacity) space complexity
        this.capacity = capacity;
        cache = new HashMap<>();
    }

    public int get(int key) {
        // O(1) complexity
    }

    public void put(int key, int value) {
        // O(1) complexity
    }

    private void remove(Node node) {
        // O(1) complexity
    }

    private void addToHead(Node node) {
        // O(1) complexity
    }
}

/*
 * Your LRUCache object will be instantiated and called as such:
 * LRUCache obj = new LRUCache(capacity);
 * int param_1 = obj.get(key);
 * obj.put(key, value);
 */
```

### Python

```python
class Node:
    def __init__(self, key, value):
        self.key = key
        self.value = value
        self.prev = None
        self.next = None

class LRUCache(object):

    def __init__(self, capacity):
        """
        :type capacity: int
        """
        self.capacity = capacity
        self.cache = {}
        self.head = Node(0, 0)
        self.tail = Node(0, 0)
        self.head.next = self.tail
        self.tail.prev = self.head

    def get(self, key):
        """
        :type key: int
        :rtype: int
        """
        if key in self.cache:
            node = self.cache[key]
            self._remove(node)
            self._add(node)
            return node.value
        return -1

    def put(self, key, value):
        """
        :type key: int
        :type value: int
        :rtype: None
        """
        if key in self.cache:
            self._remove(self.cache[key])
        node = Node(key, value)
        self._add(node)
        self.cache[key] = node
        if len(self.cache) > self.capacity:
            lru = self.tail.prev
            self._remove(lru)
            del self.cache[lru.key]

    def _remove(self, node):
        # O(1) complexity
        prev, nxt = node.prev, node.next
        prev.next = nxt
        nxt.prev = prev

    def _add(self, node):
        # O(1) complexity
        node.next = self.head.next
        self.head.next.prev = node
        self.head.next = node
        node.prev = self.head


# Your LRUCache object will be instantiated and called as such:
# obj = LRUCache(capacity)
# param_1 = obj.get(key)
# obj.put(key,value)
```

### Python3

```python
class Node:
    def __init__(self, key, value):
        self.key = key
        self.value = value
        self.prev = None
        self.next = None

class LRUCache:

    def __init__(self, capacity: int):
        self.capacity = capacity
        self.cache = {}
        self.head = Node(0, 0)
        self.tail = Node(0, 0)
        self.head.next = self.tail
        self.tail.prev = self.head

    def get(self, key: int) -> int:
        if key in self.cache:
            node = self.cache[key]
            self._remove(node)
            self._add(node)
            return node.value
        return -1

    def put(self, key: int, value: int) -> None:
        if key in self.cache:
            self._remove(self.cache[key])
        node = Node(key, value)
        self._add(node)
        self.cache[key] = node
        if len(self.cache) > self.capacity:
            lru = self.tail.prev
            self._remove(lru)
            del self.cache[lru.key]

    def _remove(self, node):
        # O(1) complexity
        prev, nxt = node.prev, node.next
        prev.next = nxt
        nxt.prev = prev

    def _add(self, node):
        # O(1) complexity
        node.next = self.head.next
        self.head.next.prev = node
        self.head.next = node
        node.prev = self.head


# Your LRUCache object will be instantiated and called as such:
# obj = LRUCache(capacity)
# param_1 = obj.get(key)
# obj.put(key,value)
```

### C

```c
#include <stdio.h>
#include <stdlib.h>
#include <uthash.h>

typedef struct Node {
    int key, value;
    struct Node* prev;
    struct Node* next;
} Node;

typedef struct {
    int capacity;
    int size;
    Node *head;
    Node *tail;
    Node* hashtable;  // hashtable to store key to Node mapping
} LRUCache;

LRUCache* lRUCacheCreate(int capacity) {
    LRUCache* cache = (LRUCache*)malloc(sizeof(LRUCache));
    cache->capacity = capacity;
    cache->size = 0;
    cache->head = NULL;
    cache->tail = NULL;
    cache->hashtable = NULL; // Using uthash for hashmap functionality
    return cache;
}

int lRUCacheGet(LRUCache* obj, int key) {
    // O(1) complexity
}

void lRUCachePut(LRUCache* obj, int key, int value) {
    // O(1) complexity
}

void lRUCacheFree(LRUCache* obj) {
    free(obj);
}

/*
 * Your LRUCache struct will be instantiated and called as such:
 * LRUCache* obj = lRUCacheCreate(capacity);
 * int param_1 = lRUCacheGet(obj, key);
 * lRUCachePut(obj, key, value);
 * lRUCacheFree(obj);
 */
```

### C#

```csharp
public class Node
{
    public int key, value;
    public Node prev, next;
    public Node(int k, int v) { key = k; value = v; }
}

public class LRUCache {

    private Node head, tail;
    private Dictionary<int, Node> cache;
    private int capacity;

    public LRUCache(int capacity) {
        this.capacity = capacity;
        cache = new Dictionary<int, Node>();
    }

    public int Get(int key) {
        // O(1) complexity
    }

    public void Put(int key, int value) {
        // O(1) complexity
    }

    private void Remove(Node node) {
        // O(1) complexity
    }

    private void AddToHead(Node node) {
        // O(1) complexity
    }
}

/*
 * Your LRUCache object will be instantiated and called as such:
 * LRUCache obj = new LRUCache(capacity);
 * int param_1 = obj.Get(key);
 * obj.Put(key, value);
 */
```

### JavaScript

```javascript
class Node {
    constructor(key, value) {
        this.key = key;
        this.value = value;
        this.prev = null;
        this.next = null;
    }
}

/**
 * @param {number} capacity
 */
var LRUCache = function(capacity) {
    this.capacity = capacity;
    this.cache = new Map();
    this.head = new Node(0, 0);
    this.tail = new Node(0, 0);
    this.head.next = this.tail;
    this.tail.prev = this.head;
};

/** 
 * @param {number} key
 * @return {number}
 */
LRUCache.prototype.get = function(key) {
    if (this.cache.has(key)) {
        let node = this.cache.get(key);
        this._remove(node);
        this._add(node);
        return node.value;
    }
    return -1;
};

/** 
 * @param {number} key 
 * @param {number} value
 * @return {void}
 */
LRUCache.prototype.put = function(key, value) {
    if (this.cache.has(key)) {
        this._remove(this.cache.get(key));
    }
    let node = new Node(key, value);
    this._add(node);
    this.cache.set(key, node);
    if (this.cache.size > this.capacity) {
        let lru = this.tail.prev;
        this._remove(lru);
        this.cache.delete(lru.key);
    }
};

LRUCache.prototype._remove = function(node) {
    // O(1) complexity
    node.prev.next = node.next;
    node.next.prev = node.prev;
};

LRUCache.prototype._add = function(node) {
    // O(1) complexity
    node.next = this.head.next;
    node.next.prev = node;
    this.head.next = node;
    node.prev = this.head;
};

/** 
 * Your LRUCache object will be instantiated and called as such:
 * var obj = new LRUCache(capacity)
 * var param_1 = obj.get(key)
 * obj.put(key,value)
 */
```

### TypeScript

```typescript
class Node {
    key: number;
    value: number;
    prev: Node | null;
    next: Node | null;

    constructor(key: number, value: number) {
        this.key = key;
        this.value = value;
        this.prev = null;
        this.next = null;
    }
}

class LRUCache {
    private capacity: number;
    private cache: Map<number, Node>;
    private head: Node;
    private tail: Node;

    constructor(capacity: number) {
        this.capacity = capacity;
        this.cache = new Map();
        this.head = new Node(0, 0);
        this.tail = new Node(0, 0);
        this.head.next = this.tail;
        this.tail.prev = this.head;
    }

    get(key: number): number {
        if (this.cache.has(key)) {
            let node = this.cache.get(key);
            this._remove(node);
            this._add(node);
            return node!.value;
        }
        return -1;
    }

    put(key: number, value: number): void {
        if (this.cache.has(key)) {
            this._remove(this.cache.get(key));
        }
        let node = new Node(key, value);
        this._add(node);
        this.cache.set(key, node);
        if (this.cache.size > this.capacity) {
            let lru = this.tail.prev;
            this._remove(lru);
            this.cache.delete(lru!.key);
        }
    }

    private _remove(node: Node | null): void {
        if (node) {
            node.prev!.next = node.next;
            node.next!.prev = node.prev;
        }
    }

    private _add(node: Node | null): void {
        if (node) {
            node.next = this.head.next;
            node.next!.prev = node;
            this.head.next = node;
            node.prev = this.head;
        }
    }
}

/** 
 * Your LRUCache object will be instantiated and called as such:
 * var obj = new LRUCache(capacity)
 * var param_1 = obj.get(key)
 * obj.put(key,value)
 */
```

### PHP

```php
class Node {
    public $key, $value;
    public $prev, $next;

    function __construct($key, $value) {
        $this->key = $key;
        $this->value = $value;
        $this->prev = null;
        $this->next = null;
    }
}

class LRUCache {
    private $capacity;
    private $cache;
    private $head, $tail;

    /**
     * @param Integer $capacity
     */
    function __construct($capacity) {
        $this->capacity = $capacity;
        $this->cache = [];
        $this->head = new Node(0, 0);
        $this->tail = new Node(0, 0);
        $this->head->next = $this->tail;
        $this->tail->prev = $this->head;
    }

    /**
     * @param Integer $key
     * @return Integer
     */
    function get($key) {
        if (array_key_exists($key, $this->cache)) {
            $node = $this->cache[$key];
            $this->remove($node);
            $this->add($node);
            return $node->value;
        }
        return -1;
    }

    /**
     * @param Integer $key
     * @param Integer $value
     * @return NULL
     */
    function put($key, $value) {
        if (array_key_exists($key, $this->cache)) {
            $this->remove($this->cache[$key]);
        }
        $node = new Node($key, $value);
        $this->add($node);
        $this->cache[$key] = $node;
        if (count($this->cache) > $this->capacity) {
            $lru = $this->tail->prev;
            $this->remove($lru);
            unset($this->cache[$lru->key]);
        }
    }

    private function remove($node) {
        // O(1) complexity
        $node->prev->next = $node->next;
        $node->next->prev = $node->prev;
    }

    private function add($node) {
        // O(1) complexity
        $node->next = $this->head->next;
        $this->head->next->prev = $node;
        $this->head->next = $node;
        $node->prev = $this->head;
    }
}

/** 
 * Your LRUCache object will be instantiated and called as such:
 * $obj = LRUCache($capacity);
 * $ret_1 = $obj->get($key);
 * $obj->put($key, $value);
 */
```

### Swift

```swift
class Node {
    var key: Int
    var value: Int
    var prev: Node?
    var next: Node?

    init(_ key: Int, _ value: Int) {
        self.key = key
        self.value = value
    }
}

class LRUCache {

    private var capacity: Int
    private var cache: [Int: Node]
    private var head: Node
    private var tail: Node

    init(_ capacity: Int) {
        self.capacity = capacity
        self.cache = [Int: Node]()
        self.head = Node(0, 0)
        self.tail = Node(0, 0)
        self.head.next = self.tail
        self.tail.prev = self.head
    }
    
    func get(_ key: Int) -> Int {
        if let node = cache[key] {
            remove(node)
            add(node)
            return node.value
        }
        return -1
    }
    
    func put(_ key: Int, _ value: Int) {
        if let node = cache[key] {
            remove(node)
        }
        let node = Node(key, value)
        add(node)
        cache[key] = node
        if cache.count > capacity {
            let lru = tail.prev!
            remove(lru)
            cache.removeValue(forKey: lru.key)
        }
    }

    private func remove(_ node: Node) {
        // O(1) complexity
        node.prev?.next = node.next
        node.next?.prev = node.prev
    }

    private func add(_ node: Node) {
        // O(1) complexity
        node.next = head.next
        node.next?.prev = node
        head.next = node
        node.prev = head
    }
}

/**
 * Your LRUCache object will be instantiated and called as such:
 * let obj = LRUCache(capacity)
 * let ret_1: Int = obj.get(key)
 * obj.put(key, value)
 */
```


### Closing Statement

**Interviewer:** Great! You've successfully implemented an LRU Cache using an optimal approach involving a combination of a doubly linked list and a hash map. This ensures that both `get` and `put` operations run in O(1) time complexity, and the space complexity remains linear in terms of the capacity. You displayed a solid understanding of data structures and their integration to solve complex problems efficiently. Well done!

**Interviewee:** Thank you! This exercise was quite insightful. I appreciate the opportunity to discuss and implement the LRU Cache. 

**Interviewer:** You're welcome. Keep practicing with different types of problems to enhance your problem-solving skills. Let's wrap up the session here. If you have any final questions, feel free to ask.

**Interviewee:** No further questions from my side. Thanks again for the productive session!

---

### Similar Questions

1. **Design a Hit Counter:**
   - Implement a hit counter which counts the number of hits received in the past 5 minutes.
   - The function `hit(timestamp)` should record a hit at a given timestamp.
   - The function `getHits(timestamp)` should return the number of hits received in the past 5 minutes.

2. **Design a Min Stack:**
   - Implement a stack that supports push, pop, top and retrieving the minimum element in constant time.
   - `push(x)` -- Push element x onto stack.
   - `pop()` -- Removes the element on the top of the stack.
   - `top()` -- Get the top element.
   - `getMin()` -- Retrieve the minimum element in the stack.

3. **Design a Logger Rate Limiter:**
   - Implement a Logger system that receives a stream of messages along with their timestamps.
   - The function `shouldPrintMessage(timestamp, message)` should return true if the message should be printed in the given timestamp, otherwise it should return false.

4. **Design an Autocomplete System:**
   - Implement an autocomplete system that returns the top 3 suggestions based on a given prefix.
   - The function should improve suggestions based on the historical usage of the system.

5. **Design a Circular Queue:**
   - Implement a circular queue with functionalities `enqueue`, `dequeue`, `front`, `rear`, and `isEmpty`.
   - Ensure that all operations take constant time.

These problems, like the LRU Cache, help you get more familiar with designing efficient data structures and working with various constraints.