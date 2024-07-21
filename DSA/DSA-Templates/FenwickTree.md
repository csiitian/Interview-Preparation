## Fenwick Tree

This is being used for Range Sum Queries.
Alternative for this is Segment Tree. ( Which takes more memory )

The major drawback for fenwick tree is that we can only use this for sum queries, while Segment Tree can be used for various type like min, max and bitwise operators.

```java []
class FenwickTree {
  int[] tree;
  int size;
  public FenwickTree(int[] arr) {
    size = arr.length + 1;
    tree = new int[size];
    buildTree(arr);
  }

  void buildTree(int[] arr) {
    for(int i=1;i<size;i++) {
      add(i, arr[i-1]);
    }
  }

  void add(int k, int x) {
    while(k < size) {
      tree[k] += x;
      k += (k & -k);
    }
  }

  int sum(int l, int r) {
    return sumHelper(r+1) - sumHelper(l);
  }

  int sumHelper(int k) {
    int s = 0;
    while(k > 0) {
      s += tree[k];
      k -= (k & -k);
    }
    return s;
  }
}

public class FenwickTreeHelper {
  public static void main(String[] args) {
    int[] arr = {2, 5, 1, 9};
    FenwickTree ft = new FenwickTree(arr);
    System.out.println(ft.sum(0, 1));
    System.out.println(ft.sum(2, 3));
    System.out.println(ft.sum(1, 1));
  }
}
```