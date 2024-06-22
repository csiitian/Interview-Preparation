package system_design.parking_lot.vehicle;

import system_design.parking_lot.ParkingSlotType;
import system_design.parking_lot.ParkingSpot;
import system_design.parking_lot.VehicleType;

public class Bike extends Vehicle {

    private final VehicleType type = VehicleType.BIKE;
    private final String licensePlate;

    public Bike(String licensePlate) {
        this.licensePlate = licensePlate;
    }

    public VehicleType getType() {
        return type;
    }

    @Override
    public boolean canFitInSpot(ParkingSpot spot) {
        return spot.getType() == ParkingSlotType.SMALL ||
                spot.getType() == ParkingSlotType.MEDIUM ||
                spot.getType() == ParkingSlotType.LARGE;
    }

    @Override
    public String getLicensePlate() {
        return licensePlate;
    }

    @Override
    public String toString() {
        return "Bike{" +
                "licensePlate='" + licensePlate + '\'' +
                '}';
    }

    @Override
    public Double getChargePerHour() {
        return 20.0;
    }
}
