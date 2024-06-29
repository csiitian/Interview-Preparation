## Dijkstra's Algorithm

It is being used to calculate shorted distance between two vertices in Graph.

It is based on Greedy Approach, where we try to pick minimum weight edges and update the adjacent vertices.
Next time we pick minimum non-visited vertex, which make sure that we pick the minimum route.

### What it is ?
> Dijkstra’s algorithm is a single-source shortest path algorithm that uses a greedy approach and calculates the shortest path from the source node to all other nodes in the graph.

```java []
class Dijkstra {

  static class Edge {

    int vertex, weight;

    Edge(int vertex, int weight) {
      this.vertex = vertex;
      this.weight = weight;
    }
  }

  public static Map<Integer, Integer> dijkstra(List<List<Integer>> edges, int start,
      int numVertices) {
    // Build the graph as an adjacency list
    Map<Integer, List<Edge>> graph = new HashMap<>();
    for (int i = 0; i < numVertices; i++) {
      graph.put(i, new ArrayList<>());
    }
    for (List<Integer> edge : edges) {
      int src = edge.get(0);
      int dest = edge.get(1);
      int weight = edge.get(2);
      graph.get(src).add(new Edge(dest, weight));
    }

    // Priority queue to store (distance, vertex) pairs
    PriorityQueue<Edge> pq = new PriorityQueue<>(Comparator.comparingInt(e -> e.weight));
    // Map to store the predecessors of each vertex
    Map<Integer, Integer> predecessors = new HashMap<>();
    // Map to store the shortest distance to each vertex
    Map<Integer, Integer> distances = new HashMap<>();
    // Initialize distances with infinity
    for (int i = 0; i < numVertices; i++) {
      distances.put(i, Integer.MAX_VALUE);
      predecessors.put(i, null);
    }
    distances.put(start, 0);
    // Add the start vertex to the priority queue
    pq.add(new Edge(start, 0));

    while (!pq.isEmpty()) {
      Edge current = pq.poll();
      int currentVertex = current.vertex;
      int currentDistance = current.weight;

      // If the popped vertex distance is greater than the recorded shortest distance, skip processing
      if (currentDistance > distances.get(currentVertex)) {
        continue;
      }

      // Process each neighboring vertex of the current vertex
      for (Edge edge : graph.get(currentVertex)) {
        int distance = currentDistance + edge.weight;
        if (distance < distances.get(edge.vertex)) {
          // Update the shortest distance
          distances.put(edge.vertex, distance);
          // Update the predecessor
          predecessors.put(edge.vertex, currentVertex);
          // Add the neighboring vertex to the priority queue
          pq.add(new Edge(edge.vertex, distance));
        }
      }
    }

    return distances;
  }
}
```

> **Time complexity:** `O((V + E) log V)`, where V is the number of vertices and E is the number of edges.

> **Auxiliary Space:** `O(V)`, where V is the number of vertices and E is the number of edges in the graph.


---

## Bellman Ford Algorithm

---

## Floyd Warshall Algorithm

### What it is ?
> Floyd-Warshall algorithm is an all-pairs shortest path algorithm that uses dynamic programming to calculate the shortest path between all pairs of nodes in the graph.

---

## A* Algorithm