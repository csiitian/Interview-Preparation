package design_problems.snake_and_ladder;

import java.util.List;

public class MainApplication {

  public static void main(String[] args) {

    GameManager gameManager = new GameManager(10, 10, List.of(new Player("Alice"), new Player("Bob"),
            new Player("Vikas")));
    gameManager.addPiece(new Piece(3, 25, PieceType.LADDER));
    gameManager.addPiece(new Piece(25, 45, PieceType.LADDER));
    gameManager.addPiece(new Piece(19, 55, PieceType.LADDER));
    gameManager.addPiece(new Piece(47, 80, PieceType.LADDER));
    gameManager.addPiece(new Piece(17, 6, PieceType.SNAKE));
    gameManager.addPiece(new Piece(45, 2, PieceType.SNAKE));
    gameManager.startGame();
  }
}
