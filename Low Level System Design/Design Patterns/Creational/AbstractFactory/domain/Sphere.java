package design_patterns.Creational.AbstractFactory.domain;

public class Sphere implements Shape {

  @Override
  public void draw() {
    System.out.println("It's sphere");
  }
}
