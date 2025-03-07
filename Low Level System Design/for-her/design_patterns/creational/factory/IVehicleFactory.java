package design_patterns.creational.factory;

public interface IVehicleFactory {
    Vehicle createVehicle(VehicleType type);
}
