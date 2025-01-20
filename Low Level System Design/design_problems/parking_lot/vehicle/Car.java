package design_problems.parking_lot.vehicle;

import design_problems.parking_lot.ParkingSlotType;
import design_problems.parking_lot.ParkingSpot;
import design_problems.parking_lot.VehicleType;

public class Car extends Vehicle {

    private final VehicleType type = VehicleType.CAR;
    private final String licensePlate;

    public Car(String licensePlate) {
        this.licensePlate = licensePlate;
    }

    public VehicleType getType() {
        return type;
    }

    @Override
    public boolean canFitInSpot(ParkingSpot spot) {
        return spot.getType() == ParkingSlotType.MEDIUM ||
                spot.getType() == ParkingSlotType.LARGE;
    }

    @Override
    public String getLicensePlate() {
        return licensePlate;
    }

    @Override
    public String toString() {
        return "Car{" +
                "licensePlate='" + licensePlate + '\'' +
                '}';
    }

    @Override
    public Double getChargePerHour() {
        return 50.0;
    }
}
