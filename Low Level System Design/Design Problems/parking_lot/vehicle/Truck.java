package system_design.parking_lot.vehicle;

import system_design.parking_lot.ParkingSlotType;
import system_design.parking_lot.ParkingSpot;
import system_design.parking_lot.VehicleType;

public class Truck extends Vehicle {

    private final VehicleType type = VehicleType.TRUCK;
    private final String licensePlate;

    public Truck(String licensePlate) {
        this.licensePlate = licensePlate;
    }

    public VehicleType getType() {
        return type;
    }

    @Override
    public boolean canFitInSpot(ParkingSpot spot) {
        return spot.getType() == ParkingSlotType.LARGE;
    }

    @Override
    public String getLicensePlate() {
        return licensePlate;
    }

    @Override
    public String toString() {
        return "Truck{" +
                "licensePlate='" + licensePlate + '\'' +
                '}';
    }

    @Override
    public Double getChargePerHour() {
        return 80.0;
    }
}
