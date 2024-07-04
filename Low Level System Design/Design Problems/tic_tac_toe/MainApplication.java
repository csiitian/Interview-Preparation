package system_design.tic_tac_toe;

import java.util.Scanner;

public class MainApplication {

  public static void main(String[] args) {
    Scanner sc = new Scanner(System.in);
    System.out.println("Select Board Size: ");
    int n = sc.nextInt();
    Board board = new Board(n);
    Player playerX = new Player(PlayerSymbol.X);
    Player playerO = new Player(PlayerSymbol.O);
    int turn = 0;
    int totalMoves = 0;
    int diag1 = 0;
    int diag2 = 0;
    int[] row = new int[n];
    int[] col = new int[n];

    System.out.println("Game Started");
    while(totalMoves != n*n) {
      if(turn == 0) {
        System.out.println("PlayerX's turn");
        int moveRow = sc.nextInt();
        int moveCol = sc.nextInt();
        if(board.board[moveRow][moveCol] != null) {
          continue;
        }
        board.board[moveRow][moveCol] = playerX.symbol;

        row[moveRow] -= 1;
        if(moveRow == moveCol) diag1 -= 1;
        if(moveRow + moveCol == n-1) diag2 -= 1;
        col[moveCol] -= 1;

        if(row[moveRow] == -n || col[moveCol] == -n || diag1 == -n || diag2 == -n) {
          board.printBoard();
          System.out.println("PlayerX won the match");
          break;
        }

        turn = (turn+1)%2;
      } else {
        System.out.println("PlayerO's turn");
        int moveRow = sc.nextInt();
        int moveCol = sc.nextInt();
        if(board.board[moveRow][moveCol] != null) {
          continue;
        }
        board.board[moveRow][moveCol] = playerO.symbol;

        row[moveRow] += 1;
        if(moveRow == moveCol) diag1 += 1;
        if(moveRow + moveCol == n-1) diag2 += 1;
        col[moveCol] += 1;

        if(row[moveRow] == n || col[moveCol] == n || diag1 == n || diag2 == n) {
          board.printBoard();
          System.out.println("PlayerO won the match");
          break;
        }

        turn = (turn+1)%2;
      }

      totalMoves++;

      board.printBoard();
    }

    System.out.println("Game Ended.");
  }
}
