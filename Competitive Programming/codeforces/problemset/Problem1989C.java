package problemset;

import java.util.Scanner;

public class Problem1989C {

  public static void main(String[] args) {
    Scanner sc = new Scanner(System.in);
    int t = sc.nextInt();
    while(t-- > 0) {
      int n = sc.nextInt();
      int[] a = new int[n];
      int[] b = new int[n];
      for(int i=0;i<n;i++) a[i] = sc.nextInt();
      for(int i=0;i<n;i++) b[i] = sc.nextInt();

      int count = 0;
      int countA = 0, countB = 0;
      for(int i=0;i<n;i++) {
        if (a[i] == 1 && b[i] == 1)
          count++;
        else if (a[i] == -1 && b[i] == -1)
          count--;
        else {
          if (a[i] == 1) {
            countA++;
          }
          if (b[i] == 1) {
            countB++;
          }
        }
      }


    }
  }
}
