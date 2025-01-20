package design_patterns.Creational.AbstractFactory.domain;

public class Cylinder implements Shape {

  @Override
  public void draw() {
    System.out.println("It's cylinder");
  }
}
