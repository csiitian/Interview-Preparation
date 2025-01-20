package design_problems.chess;

public class Board {

  private int rows = 8;
  private int cols = 8;
  private final Square[][] squares;

  // out of board pieces
  private final Piece[] outOfBoardPieces = new Piece[32];

  public Board() {
    squares = new Square[rows][cols];
    for (int i = 0; i < rows; i++) {
      for (int j = 0; j < cols; j++) {
        squares[i][j] = new Square(i, j);
      }
    }

    // initial setup
    setUpPieces();
  }

  private void setUpPieces() {
    // setPawns
    for (int i = 0; i < cols; i++) {
      squares[1][i].setPiece(new Piece('P', PieceType.PAWN, Color.BLACK));
      squares[6][i].setPiece(new Piece('P', PieceType.PAWN, Color.WHITE));
    }

    // setKing
    squares[0][4].setPiece(new Piece('K', PieceType.KING, Color.BLACK));
    squares[7][4].setPiece(new Piece('K', PieceType.KING, Color.WHITE));

    // setQueen
    squares[0][3].setPiece(new Piece('Q', PieceType.QUEEN, Color.BLACK));
    squares[7][3].setPiece(new Piece('Q', PieceType.QUEEN, Color.WHITE));

    // setBishops
    squares[0][2].setPiece(new Piece('B', PieceType.BISHOP, Color.BLACK));
    squares[0][5].setPiece(new Piece('B', PieceType.BISHOP, Color.BLACK));
    squares[7][2].setPiece(new Piece('B', PieceType.BISHOP, Color.WHITE));
    squares[7][5].setPiece(new Piece('B', PieceType.BISHOP, Color.WHITE));

    // setKnights
    squares[0][1].setPiece(new Piece('N', PieceType.KNIGHT, Color.BLACK));
    squares[0][6].setPiece(new Piece('N', PieceType.KNIGHT, Color.BLACK));
    squares[7][1].setPiece(new Piece('N', PieceType.KNIGHT, Color.WHITE));
    squares[7][6].setPiece(new Piece('N', PieceType.KNIGHT, Color.WHITE));

    // setRooks
    squares[0][0].setPiece(new Piece('R', PieceType.ROOK, Color.BLACK));
    squares[0][7].setPiece(new Piece('R', PieceType.ROOK, Color.BLACK));
    squares[7][0].setPiece(new Piece('R', PieceType.ROOK, Color.WHITE));
    squares[7][7].setPiece(new Piece('R', PieceType.ROOK, Color.WHITE));
  }

  public Square getSquare(int row, int col) {
    return squares[row][col];
  }

  public void resetBoard() {
    setUpPieces();
  }

  public void showBoard() {
    for (int i = 0; i < rows; i++) {
      for (int j = 0; j < cols; j++) {
        if (squares[i][j].isOccupied()) {
          System.out.print(squares[i][j].getPiece().getSymbol() + " ");
        } else {
          System.out.print("- ");
        }
      }
      System.out.println();
    }
  }
}
