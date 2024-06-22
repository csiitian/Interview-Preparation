package design_patterns.Creational.Factory;

public class TwoDShapeFactory {
    public static Shape CreateShape(TwoDShapeType type) {
        if(type == TwoDShapeType.RECTANGLE) return new Rectangle();
        else if(type == TwoDShapeType.CIRCLE) return new Circle();
        throw new IllegalArgumentException("Only RECTANGLE or CIRCLE shape is possible");
    }
}
