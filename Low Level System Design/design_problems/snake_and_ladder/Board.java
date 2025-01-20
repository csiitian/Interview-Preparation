package design_problems.snake_and_ladder;

import java.util.ArrayList;
import java.util.List;

public class Board {
  int size;
  int row;
  int col;
  Square[][] squares;
  List<Piece> pieces;

  Board(int row, int col) {
    this.row = row;
    this.col = col;
    this.size = row * col;
    squares = new Square[row][col];
    for (int i = 0; i < row; i++) {
      for (int j = 0; j < col; j++) {
        squares[i][j] = new Square(i, j, (i % 2 == 0) ? (i * col + j + 1) : (i * col + col - j));
      }
    }
    pieces = new ArrayList<>();
  }

  public void addPiece(Piece piece) {
    pieces.add(piece);
  }
}
