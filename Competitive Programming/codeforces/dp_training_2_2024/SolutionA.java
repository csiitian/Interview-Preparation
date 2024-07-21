package dp_training_2_2024;

import java.util.Scanner;

public class SolutionA {

  public static void main(String[] args) {
    Scanner sc = new Scanner(System.in);
    int n = sc.nextInt();
    int m = sc.nextInt();
    int k = sc.nextInt();

    int[][][] dp = new int[n + 1][m + 1][k + 1];
    dp[0][0][0] = 1;

    // Fill the DP table
    for (int i = 0; i <= n; i++) {
      for (int j = 0; j <= m; j++) {
        for (int v = 0; v <= k; v++) {
          if (i > 0 && v > 0) {
            dp[i][j][v] += dp[i - 1][j][v - 1]; // Placing a vacant space in the previous row
          }
          if (j > 0 && v > 0) {
            dp[i][j][v] += dp[i][j - 1][v - 1]; // Placing a vacant space in the previous column
          }
          if (i >= 2) {
            dp[i][j][v] += dp[i - 2][j][v]; // Placing a vertical 2x1 tile
          }
          if (j >= 2) {
            dp[i][j][v] += dp[i][j - 2][v]; // Placing a horizontal 2x1 tile
          }
        }
      }
    }

    // The answer is the sum of all ways to tile the board with up to K vacant spaces
    int result = 0;
    for (int i = 1; i <= n; i++) {
      for (int j = 1; j <= m; j++) {
        result += dp[i][j][k];
      }
    }

    System.out.println(result);
  }
}
