package design_patterns.creational.DynamicFactory.domain;

import design_patterns.creational.DynamicFactory.factory.ShapeFactory;
import design_patterns.creational.DynamicFactory.type.ShapeType;

public class Rectangle implements Shape {
  static {
    ShapeFactory.registerShape(ShapeType.RECTANGLE, Rectangle::new);
  }

  @Override
  public void draw() {
    System.out.println("It's rectangle");
  }
}
