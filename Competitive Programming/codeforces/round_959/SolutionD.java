import java.util.ArrayList;
import java.util.List;
import java.util.PriorityQueue;
import java.util.Scanner;

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

public class SolutionD {
  public static void main(String[] args) {
    Scanner sc = new Scanner(System.in);
    int t = sc.nextInt();
    while(t-- > 0) {
      int n = sc.nextInt();
      PriorityQueue<int[]> pq = new PriorityQueue<>((a1, a2) -> a1[0] - a2[0]);
      int[] a = new int[n];
      for(int i=0;i<n;i++) a[i] = sc.nextInt();
      for(int i=0;i<n;i++) {
        for(int j=i+1;j<n;j++) {
          pq.add(new int[]{Math.abs(a[i] - a[j]), i, j});
        }
      }

      List<int[]> ans = new ArrayList<int[]>();
      int start = 1;
      UnionFind uf = new UnionFind(n);
      boolean possible = true;
      while(!pq.isEmpty()) {
        int[] poll = pq.poll();
        if(uf.areAllElementsConnected()) break;
        if(uf.areConnected(poll[1], poll[2])) continue;
        if(poll[0] != start) {
          possible = false;
          break;
        }
        start++;
        ans.add(new int[]{poll[1], poll[2]});
        uf.union(poll[1], poll[2]);
      }

      if(possible) {
        System.out.println("YES");
        for(int i=0;i<ans.size();i++) {
          System.out.println(a[ans.get(i)[0]] + " " + a[ans.get(i)[1]]);
        }
      } else {
        System.out.println("NO");
      }
    }
  }
}
