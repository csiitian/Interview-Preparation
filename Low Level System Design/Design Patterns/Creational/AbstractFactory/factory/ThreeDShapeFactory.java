package design_patterns.Creational.AbstractFactory.factory;

import design_patterns.Creational.AbstractFactory.domain.Cylinder;
import design_patterns.Creational.AbstractFactory.domain.Shape;
import design_patterns.Creational.AbstractFactory.domain.Sphere;
import design_patterns.Creational.AbstractFactory.type.ShapeType;

public class ThreeDShapeFactory implements AbstractShapeFactory {

  @Override
  public Shape createShape(ShapeType type) {
      if (type == ShapeType.CYLINDER) {
          return new Cylinder();
      } else if (type == ShapeType.SPHERE) {
          return new Sphere();
      }
    throw new IllegalArgumentException("Only CYLINDER or SPHERE shape is possible");
  }
}
