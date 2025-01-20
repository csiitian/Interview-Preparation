package design_problems.parking_lot.pricing;

import design_problems.parking_lot.ParkingSpot;
import design_problems.parking_lot.vehicle.Vehicle;

public class HourlyPricingStrategy implements PricingStrategy {

    @Override
    public double calculatePrice(Vehicle vehicle, ParkingSpot parkingSpot, double hoursParked) {
        return vehicle.getChargePerHour();
    }
}
