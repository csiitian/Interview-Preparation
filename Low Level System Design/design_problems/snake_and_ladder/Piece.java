package design_problems.snake_and_ladder;

public class Piece {
  int start;
  int end;
  PieceType type;

  public Piece(int start, int end, PieceType type) {
    this.start = start;
    this.end = end;
    this.type = type;
  }
}
