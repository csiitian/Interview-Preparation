package round_956;

import java.util.Scanner;

public class CFT {
  static Scanner sc = new Scanner(System.in);

  public static int[] takeNIntInput(int n) {
    int[] inp = new int[n];
    for(int i=0;i<n;i++) inp[i] = sc.nextInt();
    return inp;
  }
}