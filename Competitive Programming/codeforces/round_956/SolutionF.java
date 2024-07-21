package round_956;

import java.util.Comparator;
import java.util.PriorityQueue;
import java.util.Scanner;

public class SolutionF {

  public static void main(String[] args) {
    Scanner sc = new Scanner(System.in);
    int t = sc.nextInt();
    while(t-- > 0) {
      int n = sc.nextInt();
      int k = sc.nextInt();
      int[] a = CFT.takeNIntInput(n);
      PriorityQueue<Integer> pq = new PriorityQueue<>(Comparator.reverseOrder());
      int ans = 0;
      for(int i=0;i<n-2;i++) {
        ans ^= a[i];
        for(int j=i+1;j<n;j++) {
          ans ^= a[j];
          pq.add(ans);
          // if(pq.size() > k) pq.poll();
          System.out.println(pq);
        }
      }
      System.out.println(pq.peek());
    }
  }
}
