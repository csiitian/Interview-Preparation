package design_patterns.Creational.AbstractFactory.factory;

import design_patterns.Creational.AbstractFactory.domain.Circle;
import design_patterns.Creational.AbstractFactory.domain.Rectangle;
import design_patterns.Creational.AbstractFactory.domain.Shape;
import design_patterns.Creational.AbstractFactory.type.ShapeType;

public class TwoDShapeFactory implements AbstractShapeFactory {

  @Override
  public Shape createShape(ShapeType type) {
      if (type == ShapeType.RECTANGLE) {
          return new Rectangle();
      } else if (type == ShapeType.CIRCLE) {
          return new Circle();
      }
    throw new IllegalArgumentException("Only RECTANGLE or CIRCLE shape is possible");
  }
}
