package design_patterns.Creational.AbstractFactory.domain;

public class Circle implements Shape {

  @Override
  public void draw() {
    System.out.println("It's circle");
  }
}
