package design_patterns.creational.DynamicFactory.factory;

import design_patterns.creational.DynamicFactory.domain.Shape;
import design_patterns.creational.DynamicFactory.type.ShapeType;

import java.util.HashMap;
import java.util.Map;
import java.util.function.Supplier;

public class ShapeFactory {
  static Map<ShapeType, Supplier<Shape>> shapeMap = new HashMap<>();

  public static void registerShape(ShapeType shapeType, Supplier<Shape> shape) {
    shapeMap.put(shapeType, shape);
  }

  public static Shape createShape(ShapeType shapeType) {
    Supplier<Shape> shape = shapeMap.get(shapeType);
    if (shape == null) {
      return null;
    }
    return shape.get();
  }
}
