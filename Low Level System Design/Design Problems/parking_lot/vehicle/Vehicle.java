package system_design.parking_lot.vehicle;

import system_design.parking_lot.ParkingSpot;

public abstract class Vehicle {
    public abstract boolean canFitInSpot(ParkingSpot spot);
    public abstract String getLicensePlate();
    public abstract String toString();
    public abstract Double getChargePerHour();
}
