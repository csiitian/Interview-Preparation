package design_patterns.creational.Factory.domain;

public class Circle implements Shape {

  @Override
  public void draw() {
    System.out.println("It's circle");
  }
}
