class SegmentTree {
  int[] tree;
  int size;
  public SegmentTree(int[] arr) {
    size = getHighestPowerOf2(arr.length);
    tree = new int[2*size];
    buildSegmentTree(arr);
  }

  void buildSegmentTree(int[] arr) {
    int n = arr.length;
    for(int i=size,j=0;i<2*size;i++,j++) {
      tree[i] = (j < n) ? arr[j] : 0;
    }
    for(int i=size-1;i>0;i--) {
      tree[i] = tree[2*i] + tree[2*i+1];
    }
  }

  int sum(int l, int r) {
    l += size;
    r += size;
    int s = 0;
    while(l <= r) {
      if(l%2 == 1) s += tree[l++];
      if(r%2 == 0) s += tree[r--];
      l /= 2;
      r /= 2;
    }
    return s;
  }

  void add(int ind, int x) {
    ind += size;
    tree[ind] += x;
    for(ind /= 2;ind > 0; ind/=2) {
      tree[ind] = tree[2*ind] + tree[2*ind+1];
    }
  }

  int getHighestPowerOf2(int n) {
    if((n & (n-1)) == 0) return n;
    return 2 * Integer.highestOneBit(n);
  }
}

public class SegmentTreeHelper {
  public static void main(String[] args) {
    int[] arr = {2, 5, 1, 9};
    SegmentTree st = new SegmentTree(arr);
    System.out.println(st.sum(0, 2));
    st.add(1, 5);
    st.add(0, 1);
    System.out.println(st.sum(0, 0));
  }
}