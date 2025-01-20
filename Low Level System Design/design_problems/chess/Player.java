package design_problems.chess;

public class Player {
  Long playerId;
  String name;
  Color color;

  public Player(Long playerId, String name, Color color) {
    this.playerId = playerId;
    this.name = name;
    this.color = color;
  }

  public Long getPlayerId() {
    return playerId;
  }

  public String getName() {
    return name;
  }

  public Color getColor() {
    return color;
  }
}
