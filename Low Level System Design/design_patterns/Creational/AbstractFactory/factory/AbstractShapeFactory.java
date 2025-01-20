package design_patterns.Creational.AbstractFactory.factory;

import design_patterns.Creational.AbstractFactory.domain.Shape;
import design_patterns.Creational.AbstractFactory.type.ShapeType;

public interface AbstractShapeFactory {

  Shape createShape(ShapeType type);
}
