import java.util.Scanner;

public class SolutionB {

  public static void main(String[] args) {
    Scanner sc = new Scanner(System.in);
    int t = sc.nextInt();

    while (t-- > 0) {
      int n = sc.nextInt();
      int m = sc.nextInt();
      int[][] a = new int[n][m];
      int[][] b = new int[n][m];
      for (int i = 0; i < n; i++) {
        char[] temp = sc.next().toCharArray();
        for(int j=0;j<m;j++) a[i][j] = temp[j] - '0';
      }
      for (int i = 0; i < n; i++) {
        char[] temp = sc.next().toCharArray();
        for(int j=0;j<m;j++) b[i][j] = temp[j] - '0';
      }
      boolean ans = true;
      for (int i = 0; i < n - 1; i++) {
        solve(a, b, i);
      }

      for (int i = 0; i < n && ans; i++) {
        for (int j = 0; j < m && ans; j++) {
          if (a[i][j] != b[i][j]) {
            ans = false;
            break;
          }
        }
      }

      if (ans) {
        System.out.println("YES");
      } else {
        System.out.println("NO");
      }
    }
  }

  private static boolean solve(int[][] a, int[][] b, int i) {
    int l = 0;
    while (l < a[0].length-1) {
      if (l < a[0].length-1 && a[i][l] == b[i][l]) {
        l++;
        continue;
      }

      a[i][l] = (a[i][l] + 1) % 3;
      a[i + 1][l+1] = (a[i + 1][l+1] + 1) % 3;
      a[i][l+1] = (a[i][l+1] + 2) % 3;
      a[i + 1][l] = (a[i + 1][l] + 2) % 3;
    }

    return a[i][l] == b[i][l];
  }
}

/*

7
3 3
0 0 0
0 0 0
0 0 0
1 1 1
1 1 1
1 1 1
4 4
0 0 0 0
0 0 0 0
0 0 0 0
0 0 0 0
2 1 0 0
1 2 0 0
0 0 1 2
0 0 2 1
4 4
1 0 2 0
1 2 0 0
1 2 1 0
0 0 0 0
0 0 0 0
1 2 0 0
2 2 0 0
0 0 0 0
3 3
0 1 2
0 1 2
0 1 2
0 1 0
1 1 1
0 1 1
8 8
0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0
1 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0
0 1 2 0 0 0 0 0
0 2 0 1 0 0 0 0
0 0 1 0 2 0 0 0
0 0 0 2 0 1 0 0
0 0 0 0 1 0 2 0
0 0 0 0 0 2 1 0
1 0 0 0 0 0 0 0
2 7
0 0 0 0 0 0 0
0 0 0 0 0 0 0
2 2 2 0 1 1 1
0 1 1 1 2 2 2
2 7
0 0 0 0 0 0 0
0 1 0 0 0 1 0
2 2 2 0 1 1 1
1 2 1 0 2 0 2


 */