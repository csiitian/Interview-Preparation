package solid_principles.LiskovSubstitutePrinciple.Solution;

public class Bicycle implements Vehicle {
  @Override
  public int noOfWheels() {
    return 2;
  }
}
