package design_problems.tic_tac_toe;

public class Board {
  int n;
  PlayerSymbol[][] board;

  Board(int n) {
    this.n = n;
    board = new PlayerSymbol[n][n];
  }

  public boolean move(int row, int col, Player player) {
    if(board[row][col] != null) return false;
    board[row][col] = player.symbol;
    return true;
  }

  public void printBoard() {
    for(int i=0;i<n;i++) {
      for(int j=0;j<n;j++) {
        if(board[i][j] == null) System.out.print("- ");
        else System.out.print(board[i][j] + " ");
      }
      System.out.println();
    }
  }
}
