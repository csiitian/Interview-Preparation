package design_patterns.creational.AbstractFactory.domain;

public class Rectangle implements Shape {

  @Override
  public void draw() {
    System.out.println("It's rectangle");
  }
}
