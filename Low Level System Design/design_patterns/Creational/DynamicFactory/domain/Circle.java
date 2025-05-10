package design_patterns.creational.DynamicFactory.domain;

import design_patterns.creational.DynamicFactory.factory.ShapeFactory;
import design_patterns.creational.DynamicFactory.type.ShapeType;

public class Circle implements Shape {
  static {
    ShapeFactory.registerShape(ShapeType.CIRCLE, Circle::new);
  }

  @Override
  public void draw() {
    System.out.println("It's circle");
  }
}
