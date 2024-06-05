# UnionFind Class

The `UnionFind` class is a data structure used for maintaining disjoint sets and efficiently merging them. It supports operations like finding whether two elements are in the same set and merging two sets into one.

## Components:

- `private int parent[];`: An array to store the parent of each element.
- `private int rank[];`: An array to store the rank of each element.
- `private int size[];`: An array to store the size of each set.
- `private int n;`: Stores the number of elements in the UnionFind data structure.
- `private int dc;`: Stores the number of disconnected components initially set to the number of elements.

## Constructor:

### `UnionFind(int _n)`

Initializes the UnionFind data structure with `_n` elements. Initializes parent, rank, and size arrays accordingly.

## Methods:

### `find(int x)`

Finds the root of the set to which element `x` belongs using path compression technique. This method optimizes the tree structure by making every node point directly to the root of the set.

### `union(int x, int y)`

Merges the sets containing elements `x` and `y` using union by rank technique. This method optimizes the tree structure by always merging the smaller set into the larger set. It also updates the rank of the root elements accordingly.

### `sortedUnion(int x, int y, boolean asc)`

Merges the sets containing elements `x` and `y` based on their ranks, with an option to sort in ascending or descending order. This method is particularly useful when you want to merge sets in a specific order based on their ranks.

### `areConnected(int x, int y)`

Checks if elements `x` and `y` belong to the same set. It returns `true` if `x` and `y` have the same root element, indicating they are in the same set.

### `getSize(int x)`

Returns the size of the set containing element `x`. This method directly accesses the size array to retrieve the size of the set.

### `countSets()`

Returns the total number of disjoint sets present. It counts the number of unique root elements in the parent array.

### `areAllElementsConnected()`

Checks if all elements in the UnionFind data structure are connected. It returns `true` if there is only one disjoint set present, meaning all elements are connected.

### `getRoots()`

Returns a list of roots of all disjoint sets. It iterates through the parent array and adds unique root elements to the list.

### `getSetElements(int x)`

Returns a list of all elements belonging to the set containing element `x`. It iterates through the parent array and adds elements with the same root as `x` to the list.

### `isSingleton(int x)`

Checks if the set containing element `x` has only one element. It returns `true` if the size of the set containing `x` is equal to 1.

### `findMinElement(int x)`

Finds the minimum element in the set containing element `x`. It iterates through the parent array to find all elements with the same root as `x` and returns the minimum among them.

### `findMaxElement(int x)`

Finds the maximum element in the set containing element `x`. It iterates through the parent array to find all elements with the same root as `x` and returns the maximum among them.

### `reset()`

Resets the UnionFind data structure to its initial state. It initializes parent, rank, and size arrays with default values and sets the number of disconnected components equal to the number of elements.

## Code
```java []
class UnionFind {
    
    private int parent[];
    private int rank[];
    private int size[];
    private int n;
    private int dc; // disconnected components
    
    UnionFind(int _n) {
        n = _n;
        dc = _n;
        rank = new int[n];
        parent = new int[n];
        size = new int[n];
        for(int i=0;i<n;i++) {
            parent[i] = i;
            size[i] = 1;
        }
    }

    int find(int x) {
        if(x == parent[x]) return x;
        return parent[x] = find(parent[x]);
    }
    
    boolean union(int x, int y) {
        int rootX = find(x);
        int rootY = find(y);
        if(rootX != rootY) {
            if(rank[rootX] < rank[rootY]) {
                parent[rootX] = rootY;
                size[rootY] += size[rootX];
            } else if(rank[rootX] > rank[rootY]) {
                parent[rootY] = rootX;
                size[rootX] += size[rootY];
            } else {
                parent[rootX] = rootY;
                rank[rootY]++;
                size[rootY] += size[rootX];
            }
            dc--;
            return true;
        }
        return false;
    }

    boolean sortedUnion(int x, int y, boolean asc) {
        int rootX = find(x);
        int rootY = find(y);
        if (rootX != rootY) {
            if (asc && rootX > rootY || 
                !asc && rootX < rootY) {
                int temp = rootX;
                rootX = rootY;
                rootY = temp;
            }
            parent[rootY] = rootX;
            size[rootX] += size[rootY];
            // there is no use of rank here
            rank[rootX] = Math.max(rank[rootX], rank[rootY] + 1);
            dc--;
            return true;
        }
        return false;
    }

    boolean areConnected(int x, int y) {
        return find(x) == find(y);
    }

    // returns the size of the disjoint set to which a specific element belongs
    int getSize(int x) {
        return size[x];
    }

    int getLargestSetSize() {
        int largestSize = 0;
        for (int root : getRoots()) {
            int setSize = getSize(root);
            largestSize = Math.max(largestSize, setSize);
        }
        return largestSize;
    }

    // returns the total number of disjoint sets present
    int countSets() {
        return dc;
    }

    boolean areAllElementsConnected() {
        return dc == 1;
    }

    List<Integer> getRoots() {
        List<Integer> roots = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            if (i == parent[i]) {
                roots.add(i);
            }
        }
        return roots;
    }

    // returns a list of all elements belonging to a particular disjoint set
    List<Integer> getSetElements(int x) {
        List<Integer> elements = new ArrayList<>();
        int rootX = find(x);
        for (int i = 0; i < n; i++) {
            if (find(i) == rootX) {
                elements.add(i);
            }
        }
        return elements;
    }

    // checks if a disjoint set contains only a single element
    boolean isSingleton(int x) {
        int rootX = find(x);
        return size[rootX] == 1;
    }

    int findMinElement(int x) {
        int rootX = find(x);
        int minElement = Integer.MAX_VALUE;
        for (int i = 0; i < n; i++) {
            if (find(i) == rootX) {
                minElement = Math.min(minElement, i);
            }
        }
        return minElement;
    }

    int findMaxElement(int x) {
        int rootX = find(x);
        int maxElement = Integer.MIN_VALUE;
        for (int i = 0; i < n; i++) {
            if (find(i) == rootX) {
                maxElement = Math.max(maxElement, i);
            }
        }
        return maxElement;
    }

    void reset() {
        for (int i = 0; i < n; i++) {
            parent[i] = i;
            rank[i] = 0;
            size[i] = 1;
        }
        dc = n;
    }
}
```
