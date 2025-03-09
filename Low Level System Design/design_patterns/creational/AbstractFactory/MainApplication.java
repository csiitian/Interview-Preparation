package design_patterns.creational.AbstractFactory;

import design_patterns.creational.AbstractFactory.domain.Shape;
import design_patterns.creational.AbstractFactory.factory.AbstractShapeFactory;
import design_patterns.creational.AbstractFactory.factory.ThreeDShapeFactory;
import design_patterns.creational.AbstractFactory.factory.TwoDShapeFactory;
import design_patterns.creational.AbstractFactory.type.ShapeType;

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
