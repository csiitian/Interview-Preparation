package design_patterns.creational.factory;

public class VehicleFactory implements IVehicleFactory {

    @Override
    public Vehicle createVehicle(VehicleType type) {
        return switch (type) {
            case VehicleType.BIKE -> new Bike();
            case VehicleType.CAR -> new Car();
            case VehicleType.TRUCK -> new Truck();
            default -> throw new RuntimeException(type + " is not supported for creating object.");
        };
    }
}
