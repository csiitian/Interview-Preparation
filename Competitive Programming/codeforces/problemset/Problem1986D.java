package problemset;

import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class Problem1986D {

  public static int solve(String str, int n) {
    if(n <= 2) return Integer.parseInt(str);
    if(str.contains("0")) return 0;
    int s = 0;
    for(int i=1;i<n-1;i++) {
      String curr = str.substring(i, i+2);
      if(str.substring(s, s+2).compareTo(curr) > 0) {
        s = i;
      }
    }
    List<Integer> tokens = new ArrayList<>();
    for(int i=0;i<n;i++) {
      if(s == i) {
        tokens.add(Integer.parseInt(str.substring(s, s+2)));
        i++;
      } else {
        tokens.add(str.charAt(i) - '0');
      }
    }
    int ans = 1;
    System.out.println(tokens);
    return ans;
  }

  public static void main(String[] args) {
    Scanner sc = new Scanner(System.in);
    int t = sc.nextInt();
    while(t-- > 0) {
      int n = sc.nextInt();
      String str = sc.next();
      int ans = solve(str, n);
      // System.out.println(ans);
    }
  }
}
