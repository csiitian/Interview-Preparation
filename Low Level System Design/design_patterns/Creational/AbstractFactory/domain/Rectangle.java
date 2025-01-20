package design_patterns.Creational.AbstractFactory.domain;

public class Rectangle implements Shape {

  @Override
  public void draw() {
    System.out.println("It's rectangle");
  }
}
