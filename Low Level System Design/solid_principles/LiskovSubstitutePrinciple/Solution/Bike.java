package solid_principles.LiskovSubstitutePrinciple.Solution;

public class Bike implements EngineVehicle {

  @Override
  public boolean hasEngine() {
    return true;
  }

  @Override
  public int noOfWheels() {
    return 2;
  }
}
