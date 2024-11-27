package system_design.car_rental_system;

import java.util.Collections;
import java.util.List;
import java.util.Objects;
import java.util.Random;
import system_design.car_rental_system.billing.Billing;
import system_design.car_rental_system.billing.BillingStatus;
import system_design.car_rental_system.exception.VehicleNotAvailableException;
import system_design.car_rental_system.location.Location;
import system_design.car_rental_system.payment.Payment;
import system_design.car_rental_system.payment.PaymentStatus;
import system_design.car_rental_system.payment.PaymentType;
import system_design.car_rental_system.reservation.Reservation;
import system_design.car_rental_system.reservation.ReservationStatus;
import system_design.car_rental_system.store.VehicleStore;
import system_design.car_rental_system.user.User;
import system_design.car_rental_system.vehicle.Vehicle;
import system_design.car_rental_system.vehicle.VehicleStatus;

public class CarRentalService {
  private List<User> users;
  private List<VehicleStore> stores;
  private List<Vehicle> vehicles;
  private List<Reservation> reservations;
  private List<Billing> billings;

  public void addUser(User user) {
    users.add(user);
  }

  public void addStore(VehicleStore store) {
    stores.add(store);
  }

  public void addVehicle(VehicleStore store, Vehicle vehicle) {
    store.addVehicles(Collections.singletonList(vehicle));
    vehicles.add(vehicle);
  }

  public Vehicle getVehicleById(Long vehicleId) {
    for (Vehicle vehicle : vehicles) {
      if (Objects.equals(vehicle.getVehicleId(), vehicleId)) {
        return vehicle;
      }
    }
    return null;
  }

  public Reservation makeReservation(User user, Vehicle vehicle, Long pickupTime, Long dropTime, Location pickupLocation, Location dropLocation)
      throws VehicleNotAvailableException {
    if (vehicle.getStatus() == VehicleStatus.AVAILABLE) {
      Reservation reservation = new Reservation(new Random().nextLong(), vehicle.getVehicleId(), user.getUserId(), pickupTime, dropTime, pickupLocation, dropLocation, ReservationStatus.PENDING);
      reservations.add(reservation);
      return reservation;
    } else {
      throw new VehicleNotAvailableException("Vehicle is not available for reservation");
    }
  }

  public Billing generateBill(Reservation reservation) {
    Long pickUpTime = reservation.getPickupTime();
    Long dropTime = reservation.getDropTime();
    long duration = dropTime - pickUpTime;
    Vehicle vehicle = getVehicleById(reservation.getVehicleId());
    Double amount = vehicle.getHourlyRate() * (duration / ( 60 * 1000 ) );
    Billing billing = new Billing(new Random().nextLong((long)1e6, (long)1e7), reservation.getReservationId(), amount, BillingStatus.PENDING);
    billings.add(billing);
    return billing;
  }

  public Payment processPayment(Billing billing, PaymentType paymentType) {
    Payment payment = new Payment(1L, billing.getBillId(), null, paymentType, billing.getAmount(), PaymentStatus.PENDING);
    // do the payment
    // on payment success
    payment.setPaymentStatus(PaymentStatus.COMPLETED);
    return payment;
  }

  public void pickUpVehicle(Reservation reservation, Vehicle vehicle) {
    reservation.setActualDropTime(System.currentTimeMillis());
    vehicle.setStatus(VehicleStatus.ON_TRIP);
  }

  public Billing getDueBill(Vehicle vehicle, Reservation reservation, boolean isDamaged, Double damagePrice, Long dropTime) {
    reservation.setActualDropTime(dropTime);
    // check due date
    double extraAmount = 0;
    if (dropTime > reservation.getDropTime()) {
      // calculate extra amount
      extraAmount = (dropTime - reservation.getDropTime()) * vehicle.getHourlyRate() * 1.5;
    }

    if (isDamaged) {
      // calculate damage amount
      extraAmount += damagePrice;
    }

    if (extraAmount > 0) {
      // generate new billing for extra amount
      Billing billing = new Billing(new Random().nextLong((long)1e6, (long)1e7), reservation.getReservationId(), extraAmount, BillingStatus.PENDING);
      billings.add(billing);
      return billing;
    }

    return null;
  }

  public void dropVehicle(Vehicle vehicle) {
    vehicle.setStatus(VehicleStatus.IN_SERVICE);
  }

  public void markVehicleAvailable(Vehicle vehicle) {
    vehicle.setStatus(VehicleStatus.AVAILABLE);
  }
}