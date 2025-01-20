package design_problems.parking_lot.vehicle;

import design_problems.parking_lot.ParkingSpot;

public abstract class Vehicle {
    public abstract boolean canFitInSpot(ParkingSpot spot);
    public abstract String getLicensePlate();
    public abstract String toString();
    public abstract Double getChargePerHour();
}
