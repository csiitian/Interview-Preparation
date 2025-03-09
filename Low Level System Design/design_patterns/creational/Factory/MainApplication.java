package design_patterns.creational.Factory;

import design_patterns.creational.Factory.domain.Shape;
import design_patterns.creational.Factory.factory.ShapeFactory;
import design_patterns.creational.Factory.type.ShapeType;

public class MainApplication {

  public static void main(String[] args) {
    ShapeFactory shapeFactory = new ShapeFactory();
    Shape circle = shapeFactory.createShape(ShapeType.CIRCLE);
    circle.draw();
    Shape rectangle = shapeFactory.createShape(ShapeType.RECTANGLE);
    rectangle.draw();
  }
}
