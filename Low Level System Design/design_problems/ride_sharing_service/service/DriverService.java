package design_problems.ride_sharing_service.service;

import design_problems.ride_sharing_service.domain.Driver;
import design_problems.ride_sharing_service.domain.Ride;
import design_problems.ride_sharing_service.domain.RideOffer;
import design_problems.ride_sharing_service.domain.RideRequest;

import java.util.HashMap;
import java.util.Map;
import java.util.Random;

public class DriverService {
    Map<String, Driver> drivers;

    DriverService() {
        drivers = new HashMap<>();
    }

    public void addDriver(Driver driver) {
        drivers.put(driver.getId(), driver);
    }

    public boolean rideOffer(RideOffer rideOffer) {
        Driver driver = drivers.get(rideOffer.getDriver().getId());
        RideRequest rideRequest = rideOffer.getRideRequest();
        boolean isAccepted = new Random().nextBoolean();
        if (isAccepted) {
            Ride ride = new Ride();
            return true;
        } else {
            rideRequest.getRejectedDrivers().add(driver);
            return false;
        }
    }
}
