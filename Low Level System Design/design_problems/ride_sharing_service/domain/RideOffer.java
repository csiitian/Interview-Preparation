package design_problems.ride_sharing_service.domain;

public class RideOffer {
    Driver driver;
    RideRequest rideRequest;

    public RideOffer(Driver driver, RideRequest rideRequest) {
        this.driver = driver;
        this.rideRequest = rideRequest;
    }

    public Driver getDriver() {
        return driver;
    }

    public RideRequest getRideRequest() {
        return rideRequest;
    }
}
