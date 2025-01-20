package design_problems.snake_and_ladder;

import java.util.List;
import java.util.UUID;

public class GameManager {
  Board board;
  Dice dice;
  List<Player> players;

  GameManager(int row, int col, List<Player> players) {
    this.board = new Board(row, col);
    this.dice = new Dice();
    this.players = players;
  }

  public void startGame() {
    while (true) {
      for (Player player : players) {
        int diceValue = dice.roll();
        int newPosition = player.position + diceValue;
        boolean ladderOrSnake = false;
        for (Piece piece : board.pieces) {
          if (piece.start == newPosition) {
            newPosition = piece.end;
            ladderOrSnake = true;
            System.out.println(
                player.name + " rolled a " + diceValue  + " and landed on a " + piece.type + " and moved to " + newPosition);
            break;
          }
        }
        if (newPosition >= board.size) {
          player.isWinner = true;
          System.out.println(player.name + " wins!");
          return;
        }
        player.position = newPosition;
        if (!ladderOrSnake) {
          System.out.println(
              player.name + " rolled a " + diceValue + " and moved to " + player.position);
        }
      }
    }
  }

  public void addPlayer(Player player) {
    players.add(player);
  }

  public void removePlayer(Player player) {
    players.remove(player);
  }

  public void addPiece(Piece piece) {
    board.addPiece(piece);
  }

  public void resetGame() {
    for (Player player : players) {
      player.position = 0;
      player.isWinner = false;
    }
  }
}