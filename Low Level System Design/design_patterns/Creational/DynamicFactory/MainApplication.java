package design_patterns.creational.DynamicFactory;

import design_patterns.creational.DynamicFactory.domain.Shape;
import design_patterns.creational.DynamicFactory.factory.ShapeFactory;
import design_patterns.creational.DynamicFactory.type.ShapeType;

public class MainApplication {

  public static void main(String[] args) {
    try {
      Class.forName("design_patterns.creational.DynamicFactory.domain.Circle");
      Class.forName("design_patterns.creational.DynamicFactory.domain.Rectangle");
    } catch (ClassNotFoundException e) {
      System.out.println("Class not found");
    }
    Shape circle = ShapeFactory.createShape(ShapeType.CIRCLE);
    circle.draw();
    Shape rectangle = ShapeFactory.createShape(ShapeType.RECTANGLE);
    rectangle.draw();
  }
}
