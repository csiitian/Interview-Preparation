package solid_principles.liskov_substitute_principle.solution;

public class Car implements EngineVehicle {

  @Override
  public boolean hasEngine() {
    return true;
  }

  @Override
  public int noOfWheels() {
    return 4;
  }
}
