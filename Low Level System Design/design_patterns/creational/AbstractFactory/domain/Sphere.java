package design_patterns.creational.AbstractFactory.domain;

public class Sphere implements Shape {

  @Override
  public void draw() {
    System.out.println("It's sphere");
  }
}
