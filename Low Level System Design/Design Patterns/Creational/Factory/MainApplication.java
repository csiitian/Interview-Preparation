package design_patterns.Creational.Factory;

public class MainApplication {
    public static void main(String[] args) {
        Shape circleShape = TwoDShapeFactory.CreateShape(TwoDShapeType.CIRCLE);
        circleShape.draw();
        Shape cylinderShape = ThreeDShapeFactory.createShape(ThreeDShapeType.CYLINDER);
        cylinderShape.draw();
    }
}
