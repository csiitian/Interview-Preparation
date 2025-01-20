package design_problems.chess;

import java.util.Scanner;

public class MainApplication {

  public static void main(String[] args) {
    Scanner sc = new Scanner(System.in);

    GameManager gameManager = new GameManager();
    Player player1 = new Player(1L, "John", Color.WHITE);
    Player player2 = new Player(2L, "Snow", Color.BLACK);
    gameManager.addPlayer(player1);
    gameManager.addPlayer(player2);

    while(true) {
      Player player = gameManager.getTurnPlayer();
      gameManager.showBoard();
      System.out.println("Player " + player.getName() + "'s turn");
      System.out.println("Enter fromX, fromY, toX, toY");
      int fromX = sc.nextInt();
      int fromY = sc.nextInt();
      int toX = sc.nextInt();
      int toY = sc.nextInt();

      gameManager.movePiece(player, fromX, fromY, toX, toY);
    }
  }
}
