package design_patterns.creational.Factory.factory;

import design_patterns.creational.Factory.domain.Circle;
import design_patterns.creational.Factory.domain.Rectangle;
import design_patterns.creational.Factory.domain.Shape;
import design_patterns.creational.Factory.type.ShapeType;

public class ShapeFactory {

  public Shape createShape(ShapeType shapeType) {
    return switch (shapeType) {
      case CIRCLE -> new Circle();
      case RECTANGLE -> new Rectangle();
      default -> null;
    };
  }
}
