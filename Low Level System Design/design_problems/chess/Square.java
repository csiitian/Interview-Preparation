package design_problems.chess;

public class Square {

  private int x;
  private int y;
  private Piece piece;

  public Square(int x, int y) {
    this.x = x;
    this.y = y;
  }

  public boolean isOccupied() {
    return piece != null;
  }

  public void setPiece(Piece piece) {
    this.piece = piece;
  }

  public void removePiece() {
    this.piece = null;
  }

  public Piece getPiece() {
    return piece;
  }

  public int getX() {
    return x;
  }

  public int getY() {
    return y;
  }
}
