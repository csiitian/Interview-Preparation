### Interviewer and Interviewee Discussion

#### Interviewer:
Let's discuss the problem of cloning a connected undirected graph. You are given the reference of a node from which you can access the entire graph. Your task is to return a deep copy of the graph. Each node in the graph contains a value and a list of its neighbors. Could you walk me through how you might initially approach this problem?

#### Interviewee:
Sure. My initial thought is to use a brute force approach to ensure every node is visited and its neighbors are copied correctly.

### Initial Brute Force Approach

#### Interviewee:
- **Step 1:** Use a data structure like a map or dictionary to store original nodes and their corresponding cloned nodes to avoid cloning the same node multiple times.
- **Step 2:** Traverse the graph starting from the given node, clone each node, and recursively clone all its neighbors.

This approach can be implemented using Depth First Search (DFS) or Breadth First Search (BFS).

### Time and Space Complexity Analysis

#### Interviewee:
- **Time Complexity:** O(V + E), where V is the number of vertices and E is the number of edges. We will visit each node and traverse each edge once.
- **Space Complexity:** O(V), which includes the space required to store the cloned nodes in a dictionary and the recursion stack if we use DFS.

#### Interviewer:
Great, now could you optimize the solution using a more efficient data structure if possible?

### Optimized Approach Using DFS and HashMap

#### Interviewee:
- We can use a HashMap to map each original node to its cloned counterpart.
- A DFS will help in navigating through the graph efficiently.

Here’s an illustration of the graph cloning process:

![Graph Cloning](https://i.imgur.com/7MaFg5P.png)

- **Step 1:** Start with the given node.
- **Step 2:** Create a copy of the node.
- **Step 3:** For each neighbor of the node, recursively create a copy if it doesn’t exist and link back to the copied node.
- **Step 4:** Use the map to ensure each node is only cloned once and to retrieve the cloned version of each neighbor.

### Code Implementation

```python
class Node:
    def __init__(self, val = 0, neighbors = None):
        self.val = val
        self.neighbors = neighbors if neighbors is not None else []

def cloneGraph(node: 'Node') -> 'Node':
    if not node:
        return node
    
    node_map = {}
    
    def dfs(node):
        if node in node_map:
            return node_map[node]
        
        clone = Node(node.val)
        node_map[node] = clone
        
        for neighbor in node.neighbors:
            clone.neighbors.append(dfs(neighbor))
        
        return clone

    return dfs(node)
```

### Explanation

#### Interviewee:
1. **Base Case:** If the input node is `None`, return `None`.
2. **HashMap:** `node_map` keeps track of visited nodes and their copies.
3. **DFS Function:** 
    - Check if the node was already cloned using `node_map`.
    - If not, clone the node.
    - Recursively clone all its neighbors and add them to the respective neighbor list of the cloned node.
4. **Return:** Start the cloning process from the given node using the DFS function.

### Complexity of Optimized Approach

- **Time Complexity:** O(V + E), same as the brute force approach.
- **Space Complexity:** O(V), storing nodes in the HashMap and recursive stack space.

#### Interviewer:
Excellent! That’s a very efficient approach. Your explanation and code are clear and well structured. Thank you!
Sure! Below are the implementations of the solution in various programming languages along with the time and space complexity annotations.

### C++

```cpp
/*
// Definition for a Node.
class Node {
public:
    int val;
    vector<Node*> neighbors;
    Node() {
        val = 0;
        neighbors = vector<Node*>();
    }
    Node(int _val) {
        val = _val;
        neighbors = vector<Node*>();
    }
    Node(int _val, vector<Node*> _neighbors) {
        val = _val;
        neighbors = _neighbors;
    }
};
*/

class Solution {
public:
    Node* cloneGraph(Node* node) {
        if (!node) return nullptr;

        unordered_map<Node*, Node*> node_map;
        
        function<Node*(Node*)> dfs = [&](Node* n) {
            if (node_map.find(n) != node_map.end()) {
                return node_map[n];
            }
            
            Node* clone = new Node(n->val);
            node_map[n] = clone;
            
            for (Node* neighbor : n->neighbors) {
                clone->neighbors.push_back(dfs(neighbor));
            }
            
            return clone;
        };

        return dfs(node);
    }
};

// Time Complexity: O(V + E)
// Space Complexity: O(V)
```

### Java

```java
/*
// Definition for a Node.
class Node {
    public int val;
    public List<Node> neighbors;
    public Node() {
        val = 0;
        neighbors = new ArrayList<Node>();
    }
    public Node(int _val) {
        val = _val;
        neighbors = new ArrayList<Node>();
    }
    public Node(int _val, ArrayList<Node> _neighbors) {
        val = _val;
        neighbors = _neighbors;
    }
}
*/

class Solution {
    public Node cloneGraph(Node node) {
        if (node == null) return null;
        
        Map<Node, Node> nodeMap = new HashMap<>();
        
        return dfs(node, nodeMap);
    }
    
    private Node dfs(Node node, Map<Node, Node> nodeMap) {
        if (nodeMap.containsKey(node)) {
            return nodeMap.get(node);
        }
        
        Node clone = new Node(node.val);
        nodeMap.put(node, clone);
        
        for (Node neighbor : node.neighbors) {
            clone.neighbors.add(dfs(neighbor, nodeMap));
        }
        
        return clone;
    }
}

// Time Complexity: O(V + E)
// Space Complexity: O(V)
```

### Python

```python
"""
# Definition for a Node.
class Node(object):
    def __init__(self, val = 0, neighbors = None):
        self.val = val
        self.neighbors = neighbors if neighbors is not None else []
"""

class Solution(object):
    def cloneGraph(self, node):
        """
        :type node: Node
        :rtype: Node
        """
        if not node:
            return node
        
        node_map = {}
        
        def dfs(n):
            if n in node_map:
                return node_map[n]
            
            clone = Node(n.val)
            node_map[n] = clone
            
            for neighbor in n.neighbors:
                clone.neighbors.append(dfs(neighbor))
            
            return clone
        
        return dfs(node)

# Time Complexity: O(V + E)
# Space Complexity: O(V)
```

### Python3

```python
"""
# Definition for a Node.
class Node:
    def __init__(self, val = 0, neighbors = None):
        self.val = val
        self.neighbors = neighbors if neighbors is not None else []
"""

from typing import Optional

class Solution:
    def cloneGraph(self, node: Optional['Node']) -> Optional['Node']:
        if not node:
            return node
        
        node_map = {}
        
        def dfs(n):
            if n in node_map:
                return node_map[n]
            
            clone = Node(n.val)
            node_map[n] = clone
            
            for neighbor in n.neighbors:
                clone.neighbors.append(dfs(neighbor))
            
            return clone
        
        return dfs(node)

# Time Complexity: O(V + E)
# Space Complexity: O(V)
```

### C

```c
/**
 * Definition for a Node.
 * struct Node {
 *     int val;
 *     int numNeighbors;
 *     struct Node** neighbors;
 * };
 */

struct Node *cloneGraph(struct Node *s) {
    if (!s) return NULL;

    struct Node* dfs_clone(struct Node* node, struct Node** node_map) {
        if (node_map[node->val]) {
            return node_map[node->val];
        }

        struct Node* clone = (struct Node*)malloc(sizeof(struct Node));
        clone->val = node->val;
        clone->numNeighbors = node->numNeighbors;
        clone->neighbors = (struct Node**)malloc(clone->numNeighbors * sizeof(struct Node*));
        node_map[node->val] = clone;

        for (int i = 0; i < node->numNeighbors; ++i) {
            clone->neighbors[i] = dfs_clone(node->neighbors[i], node_map);
        }

        return clone;
    }

    struct Node** node_map = (struct Node**)calloc(101, sizeof(struct Node*));
    struct Node* result = dfs_clone(s, node_map);
    free(node_map);
    return result;
}

// Time Complexity: O(V + E)
// Space Complexity: O(V)
```

### C#

```csharp
/*
// Definition for a Node.
public class Node {
    public int val;
    public IList<Node> neighbors;

    public Node() {
        val = 0;
        neighbors = new List<Node>();
    }

    public Node(int _val) {
        val = _val;
        neighbors = new List<Node>();
    }

    public Node(int _val, List<Node> _neighbors) {
        val = _val;
        neighbors = _neighbors;
    }
}
*/

public class Solution {
    public Node CloneGraph(Node node) {
        if (node == null) return null;

        Dictionary<Node, Node> nodeMap = new Dictionary<Node, Node>();
        
        Node DFS(Node n) {
            if (nodeMap.ContainsKey(n)) return nodeMap[n];

            Node clone = new Node(n.val);
            nodeMap[n] = clone;

            foreach (Node neighbor in n.neighbors) {
                clone.neighbors.Add(DFS(neighbor));
            }

            return clone;
        };

        return DFS(node);
    }
}

// Time Complexity: O(V + E)
// Space Complexity: O(V)
```

### JavaScript

```javascript
/**
 * // Definition for a Node.
 * function Node(val, neighbors) {
 *    this.val = val === undefined ? 0 : val;
 *    this.neighbors = neighbors === undefined ? [] : neighbors;
 * };
 */

/**
 * @param {Node} node
 * @return {Node}
 */
var cloneGraph = function(node) {
    if (!node) return null;

    let nodeMap = new Map();
    
    function dfs(n) {
        if (nodeMap.has(n)) return nodeMap.get(n);

        let clone = new Node(n.val);
        nodeMap.set(n, clone);

        for (let neighbor of n.neighbors) {
            clone.neighbors.push(dfs(neighbor));
        }

        return clone;
    }

    return dfs(node);
};

// Time Complexity: O(V + E)
// Space Complexity: O(V)
```

### TypeScript

```typescript
/**
 * Definition for Node.
 * class Node {
 *     val: number
 *     neighbors: Node[]
 *     constructor(val?: number, neighbors?: Node[]) {
 *         this.val = (val===undefined ? 0 : val)
 *         this.neighbors = (neighbors===undefined ? [] : neighbors)
 *     }
 * }
 */

function cloneGraph(node: Node | null): Node | null {
    if (!node) return null;

    let nodeMap = new Map<Node, Node>();
    
    function dfs(n: Node): Node {
        if (nodeMap.has(n)) return nodeMap.get(n)!;

        let clone = new Node(n.val);
        nodeMap.set(n, clone);

        for (let neighbor of n.neighbors) {
            clone.neighbors.push(dfs(neighbor));
        }

        return clone;
    }
    
    return dfs(node);
}

// Time Complexity: O(V + E)
// Space Complexity: O(V)
```

### PHP

```php
/**
 * Definition for a Node.
 * class Node {
 *     public $val = null;
 *     public $neighbors = null;
 *     function __construct($val = 0) {
 *         $this->val = $val;
 *         $this->neighbors = array();
 *     }
 * }
 */

class Solution {
    /**
     * @param Node $node
     * @return Node
     */
    function cloneGraph($node) {
        if (!$node) return null;

        $node_map = [];

        function dfs($n, &$node_map) {
            if (array_key_exists($n->val, $node_map)) {
                return $node_map[$n->val];
            }

            $clone = new Node($n->val);
            $node_map[$n->val] = $clone;

            foreach ($n->neighbors as $neighbor) {
                $clone->neighbors[] = dfs($neighbor, $node_map);
            }

            return $clone;
        }

        return dfs($node, $node_map);
    }
}

// Time Complexity: O(V + E)
// Space Complexity: O(V)
```

### Swift

```swift
/**
 * Definition for a Node.
 * public class Node {
 *     public var val: Int
 *     public var neighbors: [Node?]
 *     public init(_ val: Int) {
 *         self.val = val
 *         self.neighbors = []
 *     }
 * }
 */

class Solution {
    func cloneGraph(_ node: Node?) -> Node? {
        guard let node = node else { return nil }

        var nodeMap = [Int: Node]()

        func dfs(_ n: Node) -> Node {
            if let clone = nodeMap[n.val] {
                return clone
            }

            let clone = Node(n.val)
            nodeMap[n.val] = clone

            for neighbor in n.neighbors {
                if let neighbor = neighbor {
                    clone.neighbors.append(dfs(neighbor))
                }
            }

            return clone
        }

        return dfs(node)
    }
}

// Time Complexity: O(V + E)
// Space Complexity: O(V)
```

### Kotlin

```kotlin
/**
 * Definition for a Node.
 * class Node(var `val`: Int) {
 *     var neighbors: ArrayList<Node?> = ArrayList<Node?>()
 * }
 */

class Solution {
    fun cloneGraph(node: Node?): Node? {
        if (node == null) return null

        val nodeMap = HashMap<Node, Node>()

        fun dfs(n: Node): Node {
            if (nodeMap.containsKey(n)) return nodeMap[n]!!

            val clone = Node(n.`val`)
            nodeMap[n] = clone

            for (neighbor in n.neighbors) {
                clone.neighbors.add(dfs(neighbor!!))
            }

            return clone
        }

        return dfs(node)
    }
}

// Time Complexity: O(V + E)
// Space Complexity: O(V)
```

### Go

```go
/**
 * Definition for a Node.
 * type Node struct {
 *     Val int
 *     Neighbors []*Node
 * }
 */

func cloneGraph(node *Node) *Node {
    if node == nil {
        return nil
    }

    nodeMap := make(map[*Node]*Node)

    var dfs func(*Node) *Node
    dfs = func(n *Node) *Node {
        if cloned, found := nodeMap[n]; found {
            return cloned
        }

        clone := &Node{Val: n.Val}
        nodeMap[n] = clone

        for _, neighbor := range n.Neighbors {
            clone.Neighbors = append(clone.Neighbors, dfs(neighbor))
        }

        return clone
    }

    return dfs(node)
}

// Time Complexity: O(V + E)
// Space Complexity: O(V)
```

### Ruby

```ruby
# Definition for a Node.
# class Node
#     attr_accessor :val, :neighbors
#     def initialize(val = 0, neighbors = nil)
#         @val = val
#         neighbors = [] if neighbors.nil?
#         @neighbors = neighbors
#     end
# end

# @param {Node} node
# @return {Node}
def cloneGraph(node)
    return nil if node.nil?

    node_map = {}

    dfs = ->(n) {
        return node_map[n] if node_map.key?(n)
        
        clone = Node.new(n.val)
        node_map[n] = clone
        
        n.neighbors.each do |neighbor|
            clone.neighbors << dfs.call(neighbor)
        end

        clone
    }

    dfs.call(node)
end

# Time Complexity: O(V + E)
# Space Complexity: O(V)
```

### Scala

```scala
/**
 * Definition for a Node.
 * class Node(var _value: Int) {
 *   var value: Int = _value
 *   var neighbors: List[Node] = List()
 * }
 */

object Solution {
    def cloneGraph(graph: Node): Node = {
        if (graph == null) return null

        val nodeMap = scala.collection.mutable.Map[Node, Node]()

        def dfs(n: Node): Node = {
            if (nodeMap.contains(n)) {
                return nodeMap(n)
            }

            val clone = new Node(n.value)
            nodeMap.put(n, clone)

            for (neighbor <- n.neighbors) {
                clone.neighbors = dfs(neighbor) :: clone.neighbors
            }

            clone
        }

        dfs(graph)
    }
}

// Time Complexity: O(V + E)
// Space Complexity: O(V)
```

I hope these implementations are helpful for you!


### Closing Statement

Thank you for the insightful discussion on cloning a connected undirected graph. We covered various approaches, including a brute force method and an optimized approach using DFS and a hash map. We meticulously analyzed the time and space complexity for each solution and provided comprehensive code implementations across multiple programming languages. This problem is a practical application of graph traversal and helps reinforce key concepts in graph theory and deep copying of data structures.

### Similar Questions

To further enhance your understanding and practice of graph-related problems, here are a few similar questions that you can explore:

1. **Course Schedule (LeetCode #207)**
   - **Description:** Determine if it is possible to finish all courses given their prerequisites.
   - **Skill:** Graph traversal (Topological Sorting)

2. **Course Schedule II (LeetCode #210)**
   - **Description:** Return the ordering of courses you should take to finish all courses.
   - **Skill:** Graph traversal (Topological Sorting)

3. **Word Ladder (LeetCode #127)**
   - **Description:** Given two words and a dictionary, find the length of the shortest transformation sequence from the start word to the end word.
   - **Skill:** Breadth-First Search (BFS)

4. **Number of Islands (LeetCode #200)**
   - **Description:** Given a 2D grid, count the number of islands. An island is surrounded by water and is formed by connecting adjacent lands horizontally or vertically.
   - **Skill:** DFS/BFS on a 2D grid

5. **Friend Circles (LeetCode #547)**
   - **Description:** Given a matrix representing friendship relationships, return the number of friend circles in the matrix.
   - **Skill:** Graph traversal (Union-Find/DFS/BFS)

6. **Reconstruct Itinerary (LeetCode #332)**
   - **Description:** Given tickets, find the itinerary in lexical order.
   - **Skill:** Eulerian Path in a Directed Graph

7. **Clone Binary Tree with Random Pointer**
   - **Description:** Given a binary tree where each node contains a random pointer, clone the tree.
   - **Skill:** Tree traversal, Hash Map for Random Pointers

8. **Graph Valid Tree (LeetCode #261)**
   - **Description:** Given n nodes labeled from 0 to n-1 and a list of undirected edges, determine if these edges form a valid tree.
   - **Skill:** Graph traversal (Union-Find, DFS/BFS)

Each of these questions provides a unique challenge and will help you build stronger problem-solving skills for graph-related algorithms.