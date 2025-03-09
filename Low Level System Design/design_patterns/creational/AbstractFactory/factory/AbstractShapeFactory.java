package design_patterns.creational.AbstractFactory.factory;

import design_patterns.creational.AbstractFactory.domain.Shape;
import design_patterns.creational.AbstractFactory.type.ShapeType;

public interface AbstractShapeFactory {

  Shape createShape(ShapeType type);
}
