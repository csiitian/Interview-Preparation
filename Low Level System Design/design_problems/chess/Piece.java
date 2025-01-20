package design_problems.chess;

public class Piece {
  private char symbol;
  private PieceType pieceType;
  private Color color;
  private boolean isCaptured;

  public Piece(char symbol, PieceType pieceType, Color color) {
    this.symbol = symbol;
    this.pieceType = pieceType;
    this.color = color;
    this.isCaptured = false;
  }

  public void setCaptured() {
    isCaptured = true;
  }

  public Color getColor() {
    return color;
  }

  public PieceType getPieceType() {
    return pieceType;
  }

  public char getSymbol() {
    return symbol;
  }

  public boolean isCaptured() {
    return isCaptured;
  }
}
