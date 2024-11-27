package design_patterns.Creational.Factory.domain;

public class Rectangle implements Shape {

  @Override
  public void draw() {
    System.out.println("It's rectangle");
  }
}
