package design_problems.chess;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class GameManager {
  Board board;
  List<Player> playerList;
  int turn;

  GameManager() {
    board = new Board();
    playerList = new ArrayList<>();
    turn = 0;
  }

  public void addPlayer(Player player) {
    playerList.add(player);
  }

  public Player getTurnPlayer() {
    return playerList.get(turn);
  }

  public boolean movePiece(Player player, int fromX, int fromY, int toX, int toY) {
    Piece piece = board.getSquare(fromX, fromY).getPiece();
    if (piece == null) {
      System.out.println("No piece found at (" + fromX + ", " + fromY + ")");
      return false;
    }
    if (!Objects.equals(playerList.get(turn).getPlayerId(), player.getPlayerId())) {
      System.out.println("Not your turn");
      return false;
    }
    if (!Objects.equals(player.getColor(), piece.getColor())) {
      System.out.println("Not your piece");
      return false;
    }
    if (board.getSquare(toX, toY).getPiece() != null &&
        Objects.equals(board.getSquare(toX, toY).getPiece().getColor(), piece.getColor())) {
      System.out.println("Cannot capture your own piece");
      return false;
    }
    if (toX < 0 || toY < 0 || toX >= 8 || toY >= 8) {
      System.out.println("Invalid move");
      return false;
    }
    Piece opponentPiece = board.getSquare(toX, toY).getPiece();
    if (opponentPiece != null) {
      System.out.println("Captured " + opponentPiece.getPieceType() + " of " + opponentPiece.getColor());
      opponentPiece.setCaptured();
    }
    board.getSquare(toX, toY).setPiece(piece);
    board.getSquare(fromX, fromY).removePiece();
    System.out.println("Moved " + piece.getPieceType() + " from (" + fromX + ", " + fromY + ") to (" + toX + ", " + toY + ")");
    turn = (turn + 1) % playerList.size();
    return true;
  }

  public boolean isCheckmate() {
    return false;
  }

  public boolean isCheck() {
    return false;
  }

  public boolean isStalemate() {
    return false;
  }

  public boolean isDraw() {
    return false;
  }

  public void resetBoard() {
    board.resetBoard();
  }

  public void showBoard() {
    board.showBoard();
  }
}
