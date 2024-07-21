import java.io.IOException;
import java.util.HashMap;
import java.util.Scanner;

public class SolutionB {

  public static void main(String[] args) {
    Scanner sc = new Scanner(System.in);
    int t = sc.nextInt();

    while (t-- > 0) {
      long x = sc.nextLong();
      long y = sc.nextLong();
      long k = sc.nextLong();
      while (k > 0) {
        long req = ((x / y) + 1) * (y) - x;
        req = Math.max(1, req);
        req = Math.min(req, k);
        x += req;
        while (x % y == 0) {
          x /= y;
        }
        k -= req;
      }
      System.out.println(x);
    }
  }
}
