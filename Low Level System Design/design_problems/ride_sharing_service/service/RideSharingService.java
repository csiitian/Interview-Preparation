package design_problems.ride_sharing_service.service;

import design_problems.ride_sharing_service.domain.Driver;
import design_problems.ride_sharing_service.domain.Passenger;

public class RideSharingService {
    private PassengerService passengerService;
    private DriverService driverService;

    RideSharingService(PassengerService passengerService, DriverService driverService) {
        this.passengerService = passengerService;
        this.driverService = driverService;
    }

    public void addPassenger(Passenger passenger) {
        passengerService.addPassenger(passenger);
    }

    public void addDriver(Driver driver) {
        driverService.addDriver(driver);
    }


}
