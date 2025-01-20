package design_problems.snake_and_ladder;

import java.util.Random;

public class Dice {
  public int roll() {
    return new Random().nextInt(6) + 1;
  }
}
