package design_patterns.creational.Factory.domain;

public class Rectangle implements Shape {

  @Override
  public void draw() {
    System.out.println("It's rectangle");
  }
}
