package design_patterns.SolidPrinciples.LiskovSubstitutePrinciple.Problem;

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
