package solid_principles.liskov_substitute_principle.problem;

public class Car implements Vehicle {

  @Override
  public boolean hasEngine() {
    return true;
  }

  @Override
  public int noOfWheels() {
    return 4;
  }
}
