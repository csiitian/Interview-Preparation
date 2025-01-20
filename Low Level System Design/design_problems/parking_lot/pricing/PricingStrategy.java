package design_problems.parking_lot.pricing;

import design_problems.parking_lot.ParkingSpot;
import design_problems.parking_lot.vehicle.Vehicle;

public interface PricingStrategy {
    double calculatePrice(Vehicle vehicle, ParkingSpot parkingSpot, double hoursParked);
}
