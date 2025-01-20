package design_problems.snake_and_ladder;

public class Player {
  String name;
  int position;
  boolean isWinner;

  public Player(String name) {
    this.name = name;
    this.position = 0;
    this.isWinner = false;
  }
}
