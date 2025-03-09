package design_patterns.creational.AbstractFactory.factory;

import design_patterns.creational.AbstractFactory.domain.Cylinder;
import design_patterns.creational.AbstractFactory.domain.Shape;
import design_patterns.creational.AbstractFactory.domain.Sphere;
import design_patterns.creational.AbstractFactory.type.ShapeType;

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
