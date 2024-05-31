### Interviewer and Interviewee Discussion

**Interviewer:**  
So you are given a list of airline tickets where each ticket is represented by a departure and an arrival airport. Your task is to reconstruct the itinerary starting from "JFK". If there are multiple itineraries, you need to return the itinerary that has the smallest lexical order when considered as a single string. Does that make sense?

**Interviewee:**  
Yes, I understand the problem. I need to reconstruct an itinerary starting from "JFK" and ensure that if there are multiple valid itineraries, I return the one that is lexographically smallest. Also, I need to ensure that all the tickets are used exactly once.

**Interviewer:**  
Great. How would you approach solving this problem?

### Initial Thoughts: Brute Force Approach

**Interviewee:**  
For the brute force approach, I’d consider all possible permutations of the tickets and check which permutation uses all tickets exactly once and starts from "JFK". Out of those valid permutations, I would choose the one with the smallest lexicographical order.

**Interviewer:**  
Interesting. What do you think would be the time complexity of such an approach?

**Interviewee:**  
Considering all permutations would result in O((n⁴)!) complexity where n is the number of tickets since we need to check every permutation and verify if it constructs a valid itinerary starting from "JFK". This, coupled with sorting, would lead to a very high time complexity, making it inefficient for larger inputs.

**Interviewer:**  
Indeed, the brute force seems inefficient. Can you think of a more optimized approach?

### Optimizing the Approach

**Interviewee:**  
Yes, we can approach the problem using a graph and Depth-First Search (DFS). Here’s the plan:

1. **Graph Representation**: Represent the tickets as a directed graph where each airport is a node and each ticket is an edge from the departure airport to the arrival airport.
2. **Adjacency List**: Use an adjacency list to store the graph, and ensure that the list of destinations from each airport is stored in a sorted order to naturally facilitate finding the lexicographically smallest itinerary.
3. **DFS with Backtracking**: Perform a DFS starting from "JFK". During DFS, select the smallest lexicographical choice available. If a route doesn't lead to a valid itinerary (i.e., using all tickets), backtrack and try the next choice.

Let me draw an illustration for this approach:

### Illustration

Imagine our tickets are like in Example 1:
```
tickets = [["MUC","LHR"],["JFK","MUC"],["SFO","SJC"],["LHR","SFO"]]
```

1. **Graph Representation:**
   ```
   JFK -> MUC
   MUC -> LHR
   LHR -> SFO
   SFO -> SJC
   ```

2. **Graph Visualization**:
   ```
      JFK
       |
      MUC
       |
      LHR
       |
      SFO
       |
      SJC
   ```

3. **Steps during DFS**:
   - Start at "JFK".
   - Visit "MUC" from "JFK".
   - Visit "LHR" from "MUC".
   - Visit "SFO" from "LHR".
   - Visit "SJC" from "SFO".

Each step ensures lexicographic order and using each ticket only once.

### Complexity Analysis

**Time Complexity:**  
- The main operations are creating the graph, sorting the adjacency lists, and performing DFS.
- Creating the graph and sorting adjacency lists are O(n log n) where n is the number of tickets.
- DFS traversal in the worst case goes through each ticket once, making this linear with respect to the number of edges, O(n).
- Therefore, the total time complexity is O(n log n).

**Space Complexity:**  
- Storage of the graph with n tickets needs O(n) space for the adjacency list.
- The call stack for DFS will be tightly bound by the number of tickets and airports, leading to O(n) space complexity.

**Interviewer:**  
Nice job streamlining the problem using a graph and DFS to find an efficient solution. Your time and space complexity analyses are correct as well. That concludes our discussion on this problem. Thank you.
Sure, let's start with translating the efficient DFS-based approach for all the languages you provided.

### C++

```cpp
class Solution {
public:
    vector<string> findItinerary(vector<vector<string>>& tickets) {
        unordered_map<string, priority_queue<string, vector<string>, greater<string>>> graph;
        for(const auto& ticket : tickets) {
            graph[ticket[0]].push(ticket[1]);
        }
        
        vector<string> itinerary;
        stack<string> dfs;
        dfs.push("JFK");
        
        while (!dfs.empty()) {
            string node = dfs.top();
            if (graph[node].empty()) {
                itinerary.push_back(node);
                dfs.pop();
            } else {
                dfs.push(graph[node].top());
                graph[node].pop();
            }
        }
        
        reverse(itinerary.begin(), itinerary.end());
        return itinerary;
    }
};
```

### Java

```java
class Solution {
    public List<String> findItinerary(List<List<String>> tickets) {
        HashMap<String, PriorityQueue<String>> graph = new HashMap<>();
        for (List<String> ticket : tickets) {
            graph.computeIfAbsent(ticket.get(0), k -> new PriorityQueue<>()).add(ticket.get(1));
        }
        
        LinkedList<String> itinerary = new LinkedList<>();
        Stack<String> dfs = new Stack<>();
        dfs.push("JFK");
        
        while (!dfs.isEmpty()) {
            String node = dfs.peek();
            if (!graph.containsKey(node) || graph.get(node).isEmpty()) {
                itinerary.addFirst(node);
                dfs.pop();
            } else {
                dfs.push(graph.get(node).poll());
            }
        }
        
        return itinerary;
    }
}
```

### Python

```python
class Solution(object):
    def findItinerary(self, tickets):
        """
        :type tickets: List[List[str]]
        :rtype: List[str]
        """
        from collections import defaultdict, deque

        graph = defaultdict(list)
        for a, b in sorted(tickets):
            graph[a].append(b)

        itinerary, stack = [], ['JFK']
        while stack:
            while graph[stack[-1]]:
                stack.append(graph[stack[-1]].pop(0))
            itinerary.append(stack.pop())
        
        return itinerary[::-1]
```

### Python3

```python
class Solution:
    def findItinerary(self, tickets: List[List[str]]) -> List[str]:
        from collections import defaultdict

        graph = defaultdict(list)
        for a, b in sorted(tickets):
            graph[a].append(b)

        itinerary, stack = [], ['JFK']
        while stack:
            while graph[stack[-1]]:
                stack.append(graph[stack[-1]].pop(0))
            itinerary.append(stack.pop())

        return itinerary[::-1]
```

### C

```c
#include <stdlib.h>
#include <string.h>

void dfs(char *node, int *graph, int *out_degree, char **itinerary, int *itinerary_index) {
    while(out_degree[node[0]]) {
        char *next_node = graph[node[0] * 26 + (--out_degree[node[0]])];
        dfs(next_node, graph, out_degree, itinerary, itinerary_index);
    }
    itinerary[(*itinerary_index)--] = node;
}

/**
 * Note: The returned array must be malloced, assume caller calls free().
 */
char** findItinerary(char*** tickets, int ticketsSize, int* ticketsColSize, int* returnSize) {
    int graph[26*26] = {0};
    int out_degree[26] = {0};
    char ***sorted_tickets = malloc(ticketsSize * sizeof(char**));
    for (int i = 0; i < ticketsSize; ++i)
        sorted_tickets[i] = tickets[i];
    
    char **itinerary = malloc((ticketsSize + 1) * sizeof(char*));
    int itinerary_index = ticketsSize;
    
    dfs("JFK", graph, out_degree, itinerary, &itinerary_index);
    *returnSize = ticketsSize + 1;
    return itinerary;
}
```

### C#

```csharp
public class Solution {
    public IList<string> FindItinerary(IList<IList<string>> tickets) {
        var graph = new Dictionary<string, SortedSet<string>>();
        foreach (var ticket in tickets) {
            if (!graph.ContainsKey(ticket[0])) {
                graph[ticket[0]] = new SortedSet<string>(StringComparer.Ordinal);
            }
            graph[ticket[0]].Add(ticket[1]);
        }

        var itinerary = new List<string>();
        var stack = new Stack<string>();
        stack.Push("JFK");

        while (stack.Count > 0) {
            var node = stack.Peek();

            if (!graph.ContainsKey(node) || graph[node].Count == 0) {
                itinerary.Add(stack.Pop());
            } else {
                var nextNode = graph[node].Min;
                graph[node].Remove(nextNode);
                stack.Push(nextNode);
            }
        }

        itinerary.Reverse();
        return itinerary;
    }
}
```

### JavaScript

```javascript
/**
 * @param {string[][]} tickets
 * @return {string[]}
 */
var findItinerary = function(tickets) {
    const graph = new Map();
    for (const [from, to] of tickets) {
        if (!graph.has(from)) graph.set(from, []);
        graph.get(from).push(to);
    }
    for (const edges of graph.values()) edges.sort();

    const itinerary = [];
    const dfs = (node) => {
        const dests = graph.get(node) || [];
        while (dests.length) dfs(dests.shift());
        itinerary.push(node);
    };
    
    dfs("JFK");
    return itinerary.reverse();
};
```

### TypeScript

```typescript
function findItinerary(tickets: string[][]): string[] {
    const graph = new Map<string, string[]>();
    for (const [from, to] of tickets) {
        if (!graph.has(from)) graph.set(from, []);
        graph.get(from).push(to);
    }
    for (const [from, edges] of graph.entries()) {
        edges.sort();
    }

    const itinerary: string[] = [];
    const dfs = (node: string) => {
        const dests = graph.get(node) || [];
        while (dests.length) dfs(dests.shift());
        itinerary.push(node);
    };

    dfs("JFK");
    return itinerary.reverse();
}
```

### PHP

```php
class Solution {

    /**
     * @param String[][] $tickets
     * @return String[]
     */
    function findItinerary($tickets) {
        $graph = [];
        foreach($tickets as list($from, $to)) {
            $graph[$from][] = $to;
        }
        foreach($graph as $from => &$tos) {
            sort($tos);
        }
        $itinerary = [];
        $stack = ['JFK'];
        while (!empty($stack)) {
            $node = end($stack);
            if (empty($graph[$node])) {
                $itinerary[] = array_pop($stack);
            } else {
                $stack[] = array_shift($graph[$node]);
            }
        }
        return array_reverse($itinerary);
    }
}
```

### Swift

```swift
class Solution {
    func findItinerary(_ tickets: [[String]]) -> [String] {
        var graph = [String: [String]]()
        for ticket in tickets {
            graph[ticket[0], default: [String]()].append(ticket[1])
        }
        for key in graph.keys {
            graph[key]?.sort()
        }
        
        var itinerary = [String]()
        var stack = ["JFK"]
        
        while !stack.isEmpty {
            let node = stack.last!
            if let targets = graph[node], !targets.isEmpty {
                stack.append(graph[node]!.removeFirst())
            } else {
                itinerary.append(stack.removeLast())
            }
        }
        
        return itinerary.reversed()
    }
}
```

### Kotlin

```kotlin
class Solution {
    fun findItinerary(tickets: List<List<String>>): List<String> {
        val graph = HashMap<String, PriorityQueue<String>>()
        for (ticket in tickets) {
            graph.computeIfAbsent(ticket[0]) { PriorityQueue<String>() }.add(ticket[1])
        }
        
        val itinerary = LinkedList<String>()
        val stack = Stack<String>()
        stack.push("JFK")
        
        while (stack.isNotEmpty()) {
            val node = stack.peek()
            if (!graph.containsKey(node) || graph[node]!!.isEmpty()) {
                itinerary.addFirst(node)
                stack.pop()
            } else {
                stack.push(graph[node]!!.poll())
            }
        }
        
        return itinerary.toList()
    }
}
```

### Dart

```dart
class Solution {
  List<String> findItinerary(List<List<String>> tickets) {
    Map<String, List<String>> graph = {};
    for (var ticket in tickets) {
      (graph.putIfAbsent(ticket[0], () => [])).add(ticket[1]);
    }
    for (var key in graph.keys) {
      graph[key]!.sort();
    }

    List<String> itinerary = [];
    List<String> stack = ['JFK'];

    while (stack.isNotEmpty) {
      String node = stack.last;
      if (graph[node] == null || graph[node]!.isEmpty) {
        itinerary.add(stack.removeLast());
      } else {
        stack.add(graph[node]!.removeAt(0));
      }
    }

    return itinerary.reversed.toList();
  }
}
```

### Go

```go
import "sort"

func findItinerary(tickets [][]string) []string {
    graph := make(map[string][]string)
    for _, ticket := range tickets {
        graph[ticket[0]] = append(graph[ticket[0]], ticket[1])
    }
    for _, dests := range graph {
        sort.Strings(dests)
    }
    
    itinerary := []string{}
    stack := []string{"JFK"}
    
    for len(stack) > 0 {
        node := stack[len(stack) - 1]
        if len(graph[node]) == 0 {
            itinerary = append(itinerary, node)
            stack = stack[:len(stack) - 1]
        } else {
            stack = append(stack, graph[node][0])
            graph[node] = graph[node][1:]
        }
    }
    
    for i, j := 0, len(itinerary) - 1; i < j; i, j = i + 1, j - 1 {
        itinerary[i], itinerary[j] = itinerary[j], itinerary[i]
    }
    
    return itinerary
}
```

### Ruby

```ruby
def find_itinerary(tickets)
    graph = Hash.new { |h, k| h[k] = [] }
    tickets.each { |from, to| graph[from] << to }
    graph.each_value(&:sort!)
    
    itinerary = []
    stack = ["JFK"]
    
    while !stack.empty?
        while graph[stack.last].any?
            stack.push(graph[stack.last].shift)
        end
        itinerary.unshift(stack.pop)
    end
    
    return itinerary
end
```

### Scala

```scala
object Solution {
    def findItinerary(tickets: List[List[String]]): List[String] = {
        val graph = scala.collection.mutable.Map[String, scala.collection.mutable.PriorityQueue[String]]()
        tickets.foreach(ticket => {
            graph.getOrElseUpdate(ticket(0), scala.collection.mutable.PriorityQueue.empty(Ordering[String].reverse)).enqueue(ticket(1))
        })
        
        val itinerary = new scala.collection.mutable.ListBuffer[String]()
        val stack = new scala.collection.mutable.Stack[String]()
        stack.push("JFK")
        
        while (stack.nonEmpty) {
            val node = stack.top
            if (graph.contains(node) && graph(node).nonEmpty) {
                stack.push(graph(node).dequeue())
            } else {
                itinerary.prepend(stack.pop)
            }
        }
        
        itinerary.toList
    }
}
```

### Rust

```rust
use std::collections::{BTreeMap, VecDeque};

impl Solution {
    pub fn find_itinerary(tickets: Vec<Vec<String>>) -> Vec<String> {
        let mut graph = BTreeMap::new();
        for ticket in tickets {
            graph.entry(ticket[0].clone()).or_insert_with(VecDeque::new).push_back(ticket[1].clone());
        }
        
        let mut itinerary = vec![];
        let mut stack = vec!["JFK".to_string()];
        
        while let Some(node) = stack.last() {
            if let Some(dests) = graph.get_mut(node) {
                if !dests.is_empty() {
                    stack.push(dests.pop_front().unwrap());
                    continue;
                }
            }
            itinerary.push(stack.pop().unwrap());
        }
        
        itinerary.reverse();
        itinerary
    }
}
```

### Racket

```racket
(define/contract (find-itinerary tickets)
  (-> (listof (listof string?)) (listof string?))
  (let* ([graph (make-hash)]
         [stack (list "JFK")]
         [result '()])
    (for-each (lambda ([from-to (list from to)])
                (hash-update! graph from cons to))
              tickets)
    (let loop ([s stack])
      (let ([node (first s)])
        (if (null? (hash-ref graph node '()))
            (begin
              (set! result (cons node result))
              (if (null? (rest s))
                  (reverse result)
                  (loop (rest s))))
            (begin
              (set! s (cons (first (hash-ref graph node)) s))
              (hash-set! graph node (rest (hash-ref graph node)))
              (loop s))))))
```

### Erlang

```erlang
-spec find_itinerary(Tickets :: [[unicode:unicode_binary()]]) -> [unicode:unicode_binary()].
find_itinerary(Tickets) ->

    Graph = build_graph(Tickets, #{}, []),
    Itinerary = find_itinerary_([<<"JFK">>], [], Graph),
    lists:reverse(Itinerary).

% Helper function to build adjacency list
build_graph([Ticket | Rest], Graph) ->
    build_graph(Rest, maps:update_with_element (hd(Ticket), lists:append({tl(Ticket)}), Graph));
build_graph([], Graph) -> Graph.

% Helper function for DFS
find_itinerary_([Node|Rest], Itinerary, Graph) ->
    case maps:get(Node, Graph) of
        [] -> find_itinerary_(Rest, [Node | Itinerary], maps:remove(Node, Graph));
        [Next | More] -> 
            find_itinerary_([Next,Node|Rest], Itinerary, maps:update(Node, More, Graph))
    end;
find_itinerary_([], Itinerary, _Graph) -> Itinerary.
```

### Elixir

```elixir
defmodule Solution do
  @spec find_itinerary(tickets :: [[String.t]]) :: [String.t]
  def find_itinerary(tickets) do
    graph = Enum.reduce(tickets, %{}, fn [from, to], acc ->
      Map.update(acc, from, [to], fn dests -> Enum.sort([to | dests]) end)
    end)

    dfs(["JFK"], graph, [], length(tickets) + 1)
    |> Enum.reverse()
  end

  defp dfs([h | t] = stack, graph, itinerary, itinerary_length) do
    if length(itinerary) == itinerary_length do
      itinerary
    else
      case Map.get(graph, h, []) do
        [] -> dfs(t, graph, [h | itinerary], itinerary_length)
        [n | rest] -> 
          graph = Map.put(graph, h, rest)
          dfs([n | stack], graph, itinerary, itinerary_length)
      end
    end
  end
end
```

These code snippets cover the various languages, making sure to wrap everything within the appropriate method and optimizing using the graph-based DFS approach to solve the given problem efficiently.


### Closing Statement

In our discussion, we explored the problem of reconstructing an itinerary from a list of airline tickets, ensuring that the itinerary starts from "JFK" and is lexicographically smallest. We reviewed the initial brute force approach and identified its inefficiency. We then optimized our approach using graph representation and Depth-First Search (DFS) with backtracking to ensure both correctness and efficiency.

By applying DFS on a graph and leveraging priority queues or sorting for lexicographical order, we were able to design a solution that effectively handles the constraints and requirements of the problem. This approach minimizes time complexity to O(n log n) due to sorting operations and maintains O(n) space complexity for graph storage and recursion stack.

The provided code snippets across multiple programming languages demonstrated how the solution can be implemented in diverse environments, ensuring a comprehensive understanding of the algorithm and its application.

### Similar Questions

Here are some similar questions you might find interesting and worth practicing:

1. **Word Ladder II**: Find all shortest transformation sequences from start to end.
2. **Course Schedule II**: Determine a possible order of courses to be taken given prerequisites.
3. **Alien Dictionary**: Given a sorted dictionary of an alien language, determine the order of characters.
4. **Eulerian Path in a Directed Graph**: Find an Eulerian path in a directed graph (if it exists).
5. **The Maze II**: Find the shortest path for a ball in a maze with holes.
6. **Reconstruct Original Digits from English**: Find the original digits of a given jumbled English lowercase string representing digits 0-9.
7. **Find the City With the Smallest Number of Neighbors at a Threshold Distance**: Solve using the Floyd-Warshall or Dijkstra's algorithm.
8. **Network Delay Time**: Find the time it takes for all nodes to receive a signal sent from a starting node.

By practicing these questions, you can further improve your understanding of graph traversal algorithms and problem-solving skills related to pathfinding and itinerary reconstruction in various contexts.