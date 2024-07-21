## Segment Tree

This is one of the less used DSA, most of the competitive programmers don't learn it because there is less probability of asking question on this.
Just to assure you, it is not hard but most of the people never try to learn this, all you need to do is watch atleast 3-4 videos on segment tree from diff-2 youtubers and then try to implement by your own and that's it.

You can use below template for solving problems. There are couple of things that are diff-2.
1. I am not using recursion to build tree.
2. I am using one template to build all type of segment tree dynamically.

```java []
class SegmentTree {
  enum Type {
    SUM, MIN, MAX, OR, AND, XOR;
  }

  int[] tree;
  int size;
  Type type;
  public SegmentTree(int[] arr, String type) {
    size = getHighestPowerOf2(arr.length);
    tree = new int[2*size];
    this.type = Type.valueOf(type);
    buildSegmentTree(arr);
  }

  void buildSegmentTree(int[] arr) {
    int n = arr.length;
    for(int i=size,j=0;i<2*size;i++,j++) {
      tree[i] = (j < n) ? arr[j] : 0;
    }
    for(int i=size-1;i>0;i--) {
      tree[i] = performOperation(tree[2*i], tree[2*i+1]);
    }
  }

  int performOperation(int leftChild, int rightChild) {
    switch(type) {
      case SUM: return leftChild + rightChild;
      case MAX: return Math.max(leftChild, rightChild);
      case MIN: return Math.min(leftChild, rightChild);
      case AND: return leftChild == 0 ? rightChild : leftChild & rightChild;
      case OR: return leftChild | rightChild;
      case XOR: return leftChild ^ rightChild;
      default: return leftChild + rightChild;
    }
  }

  int query(int l, int r) {
    l += size;
    r += size;
    int s = 0;
    while(l <= r) {
      if(l%2 == 1) s = performOperation(s, tree[l++]);
      if(r%2 == 0) s = performOperation(s, tree[r--]);
      l /= 2;
      r /= 2;
    }
    return s;
  }

  void add(int ind, int x) {
    ind += size;
    tree[ind] += x;
    for(ind /= 2;ind > 0; ind/=2) {
      tree[ind] = performOperation(tree[2*ind], tree[2*ind+1]);
    }
  }

  int getHighestPowerOf2(int n) {
    if((n & (n-1)) == 0) return n;
    return 2 * Integer.highestOneBit(n);
  }
}

public class SegmentTreeHelper {
  public static void main(String[] args) {
    int[] arr = {3, 5, 1, 9};
    SegmentTree st = new SegmentTree(arr, "AND");
    System.out.println(st.query(0, 1));
    st.add(1, 5);
    st.add(0, 10);
    System.out.println(st.query(0, 1));
  }
}
```