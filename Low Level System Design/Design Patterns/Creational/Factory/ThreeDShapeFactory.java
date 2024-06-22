package design_patterns.Creational.Factory;

public class ThreeDShapeFactory {
    public static Shape createShape(ThreeDShapeType type) {
        if(type == ThreeDShapeType.CYLINDER) return new Cylinder();
        else if(type == ThreeDShapeType.SPHERE) return new Sphere();
        throw new IllegalArgumentException("Only CYLINDER or SPHERE shape is possible");
    }
}
