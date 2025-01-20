package design_patterns.Creational.Factory.factory;

import design_patterns.Creational.Factory.domain.Circle;
import design_patterns.Creational.Factory.domain.Rectangle;
import design_patterns.Creational.Factory.domain.Shape;
import design_patterns.Creational.Factory.type.ShapeType;

public class ShapeFactory {

  public Shape createShape(ShapeType shapeType) {
    return switch (shapeType) {
      case CIRCLE -> new Circle();
      case RECTANGLE -> new Rectangle();
      default -> null;
    };
  }
}
