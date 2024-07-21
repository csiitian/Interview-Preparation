import java.util.Scanner;

public class SolutionC {

  public static void main(String[] args) {
    Scanner sc = new Scanner(System.in);
    int t = sc.nextInt();
    while (t-- > 0) {
      int n = sc.nextInt();
      int[] a = new int[n];
      int[] b = new int[n];
      int[] c = new int[n];
      long sum = 0;
      for (int i = 0; i < n; i++) {
        a[i] = sc.nextInt();
        sum += (long) a[i];
      }
      for (int i = 0; i < n; i++) {
        b[i] = sc.nextInt();
      }
      for (int i = 0; i < n; i++) {
        c[i] = sc.nextInt();
      }

      int[] ans = solve(a, b, c, sum);
      if(ans.length > 1) {
        printArray(ans, 0, 2, 4);
        continue;
      }
      ans = solve(a, c, b, sum);
      if(ans.length > 1) {
        printArray(ans, 0, 4, 2);
        continue;
      }
      ans = solve(b, a, c, sum);
      if(ans.length > 1) {
        printArray(ans, 2, 0, 4);
        continue;
      }
      ans = solve(b, c, a, sum);
      if(ans.length > 1) {
        printArray(ans, 4, 0, 2);
        continue;
      }
      ans = solve(c, a, b, sum);
      if(ans.length > 1) {
        printArray(ans, 2, 4, 0);
        continue;
      }
      ans = solve(c, b, a, sum);
      if(ans.length > 1) {
        printArray(ans, 4, 2, 0);
        continue;
      }
      System.out.println("-1");
    }
  }

  private static void printArray(int[] ans, int a, int b, int c) {
    System.out.println(ans[a] + " " + ans[a+1] + " " + ans[b] + " " + ans[b+1] + " " + ans[c] + " " + ans[c+1]);
  }

  private static int[] solve(int[] a, int[] b, int[] c, long sum) {
    int n = a.length;
    int l = 0;
    int r = n - 1;
    long min = Math.ceilDiv(sum, 3);
    long pre = 0;
    int[] ans = new int[6];
    while (l <= r) {
      pre += (long) a[l++];
      if (pre >= min) {
        break;
      }
    }
    ans[0] = 1;
    ans[1] = l;
    pre = 0;
    while (l <= r) {
      pre += (long) c[r--];
      if (pre >= min) {
        break;
      }
    }
    ans[4] = r+2;
    ans[5] = n;
    pre = 0;
    int preL = l+1;
    while (l <= r) {
      pre += (long) b[l++];
    }
    ans[2] = preL;
    ans[3] = r+1;

    if (pre >= min) {
      return ans;
    } else {
      return new int[]{-1};
    }
  }
}
