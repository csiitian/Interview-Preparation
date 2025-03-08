package design_patterns.creational.factory;

public class MainApplication {
    public static void main(String[] args) {
        IVehicleFactory vehicleFactory = new VehicleFactory();
        Vehicle car = vehicleFactory.createVehicle(VehicleType.CAR);
        car.print();
        Vehicle bike = vehicleFactory.createVehicle(VehicleType.BIKE);
        bike.print();
        Vehicle truck = vehicleFactory.createVehicle(VehicleType.TRUCK);
        truck.print();
    }
}
