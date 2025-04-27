package design_problems.car_rental_system;

import design_problems.car_rental_system.domain.Billing;
import design_problems.car_rental_system.exception.VehicleNotAvailableException;
import design_problems.car_rental_system.domain.Location;
import design_problems.car_rental_system.domain.Payment;
import design_problems.car_rental_system.domain.PaymentStatus;
import design_problems.car_rental_system.domain.PaymentType;
import design_problems.car_rental_system.domain.Reservation;
import design_problems.car_rental_system.domain.VehicleStore;
import design_problems.car_rental_system.domain.User;
import design_problems.car_rental_system.domain.UserType;
import design_problems.car_rental_system.domain.Vehicle;
import design_problems.car_rental_system.domain.VehicleStatus;
import design_problems.car_rental_system.domain.VehicleType;

public class MainApplication {

  public static void main(String[] args) {
    CarRentalService carRentalService = new CarRentalService();

    User user = new User(1L, "John", "john@gmail.com", "1234567890", "123, New Street, New York",
        "DL123", "password", UserType.CUSTOMER);

    VehicleStore store = new VehicleStore(1L, "Store1", "123, New Street, New York", "New York",
        "New York", "123456", "USA", true);

    Vehicle vehicle = new Vehicle(1L, 1L, "KA01AB1234", "Toyota", 4, VehicleType.CAR,
        VehicleStatus.AVAILABLE, 20.0, null);

    carRentalService.addUser(user);
    carRentalService.addStore(store);
    carRentalService.addVehicle(store, vehicle);

    try {
      Reservation reservation = carRentalService.makeReservation(user, vehicle, 1L, 1L, new Location(1L, "Shahbadd"),
          new Location(2L, "Shahpura"));

      Billing billing = carRentalService.generateBill(reservation);
      Payment payment = carRentalService.processPayment(billing, PaymentType.UPI);

      if (payment.getStatus() == PaymentStatus.COMPLETED) {
        billing.confirmPayment();
        reservation.confirmReservation();
      }

      // pick up vehicle
      carRentalService.pickUpVehicle(reservation, vehicle);

      Billing dueBill = carRentalService.getDueBill(vehicle, reservation, true, 1000.50, System.currentTimeMillis());
      if (dueBill != null) {
        carRentalService.processPayment(dueBill, PaymentType.CASH);
      }

      carRentalService.dropVehicle(vehicle);

      // make vehicle available
      carRentalService.markVehicleAvailable(vehicle);
    } catch (VehicleNotAvailableException e) {
      System.out.println(e.getMessage());
    }
  }
}
