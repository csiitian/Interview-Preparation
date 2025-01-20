package design_patterns.SolidPrinciples.LiskovSubstitutePrinciple.Problem;

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
