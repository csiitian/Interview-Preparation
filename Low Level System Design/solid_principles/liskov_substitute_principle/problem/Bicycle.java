package solid_principles.liskov_substitute_principle.problem;

public class Bicycle implements Vehicle {

  @Override
  public boolean hasEngine() {
    throw new RuntimeException("Bicycle don't have engine.");
  }

  @Override
  public int noOfWheels() {
    return 2;
  }
}
