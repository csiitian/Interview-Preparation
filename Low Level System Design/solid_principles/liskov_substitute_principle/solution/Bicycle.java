package solid_principles.liskov_substitute_principle.solution;

public class Bicycle implements Vehicle {
  @Override
  public int noOfWheels() {
    return 2;
  }
}
