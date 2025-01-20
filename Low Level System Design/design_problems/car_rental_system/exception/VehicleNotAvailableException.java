package design_problems.car_rental_system.exception;

public class VehicleNotAvailableException extends Exception {
  public VehicleNotAvailableException(String message) {
    super(message);
  }
}
