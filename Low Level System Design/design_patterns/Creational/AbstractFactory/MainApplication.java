package design_patterns.Creational.AbstractFactory;

import design_patterns.Creational.AbstractFactory.domain.Shape;
import design_patterns.Creational.AbstractFactory.factory.AbstractShapeFactory;
import design_patterns.Creational.AbstractFactory.factory.ThreeDShapeFactory;
import design_patterns.Creational.AbstractFactory.factory.TwoDShapeFactory;
import design_patterns.Creational.AbstractFactory.type.ShapeType;

public class MainApplication {

  public static void main(String[] args) {
    AbstractShapeFactory twoDShapeFactory = new TwoDShapeFactory();
    Shape circleShape = twoDShapeFactory.createShape(ShapeType.CIRCLE);
    circleShape.draw();

    AbstractShapeFactory threeDShapeFactory = new ThreeDShapeFactory();
    Shape cylinderShape = threeDShapeFactory.createShape(ShapeType.CYLINDER);
    cylinderShape.draw();
  }
}
