package design_problems.ride_sharing_service.service;

import design_problems.ride_sharing_service.domain.Passenger;

import java.util.HashMap;
import java.util.Map;

public class PassengerService {
    Map<String, Passenger> passengers;

    PassengerService() {
        passengers = new HashMap<>();
    }

    public void addPassenger(Passenger passenger) {
        passengers.put(passenger.getId(), passenger);
    }
}
