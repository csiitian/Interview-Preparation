package design_problems.ride_sharing_service.domain;

import java.util.List;

public class RideRequest {
    private int rideId;
    private int passengerId;
    private Location source;
    private Location destination;
    private List<Driver> rejectedDrivers;

    public Location getSource() {
        return source;
    }

    public List<Driver> getRejectedDrivers() {
        return rejectedDrivers;
    }
}
