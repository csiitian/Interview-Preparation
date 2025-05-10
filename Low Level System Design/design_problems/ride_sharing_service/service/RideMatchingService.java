package design_problems.ride_sharing_service.service;

import design_problems.ride_sharing_service.domain.Driver;
import design_problems.ride_sharing_service.domain.RideOffer;
import design_problems.ride_sharing_service.domain.RideRequest;

import static design_problems.ride_sharing_service.utils.Utils.getDistance;

import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

public class RideMatchingService {
    private DriverService driverService;

    RideMatchingService(DriverService driverService) {
        this.driverService = driverService;
    }

    public void matchRide(RideRequest rideRequest) {
        List<Driver> drivers = new ArrayList<>(driverService.drivers.values());
        Optional<Driver> closestDriver = drivers.stream()
                .filter(driver -> !rideRequest.getRejectedDrivers().contains(driver))
                .filter(Driver::isAvailable)
                .min((d1, d2) -> (int) (getDistance(d1.getLocation(), rideRequest.getSource()) -
                        getDistance(d2.getLocation(), rideRequest.getSource())));
        // send a ride request to a driver
        closestDriver.ifPresent(driver -> {
            driverService.rideOffer(new RideOffer(driver, rideRequest));
        });
    }
}
