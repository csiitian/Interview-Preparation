package design_problems.car_rental_system.domain;

public class Reservation {
  Long reservationId;
  Long vehicleId;
  Long userId;
  Long pickupTime;
  Long dropTime;
  Location pickupLocation;
  Location dropLocation;
  Long actualPickupTime;
  Long actualDropTime;
  ReservationStatus status;

  public Reservation() {
  }

  public Reservation(Long reservationId, Long vehicleId, Long userId, Long pickupTime, Long dropTime, Location pickupLocation, Location dropLocation, ReservationStatus status) {
    this.reservationId = reservationId;
    this.vehicleId = vehicleId;
    this.userId = userId;
    this.pickupTime = pickupTime;
    this.dropTime = dropTime;
    this.pickupLocation = pickupLocation;
    this.dropLocation = dropLocation;
    this.status = status;
  }

  public Long getReservationId() {
    return reservationId;
  }

  public Long getVehicleId() {
    return vehicleId;
  }

  public Long getUserId() {
    return userId;
  }

  public Long getPickupTime() {
    return pickupTime;
  }

  public Long getDropTime() {
    return dropTime;
  }

  public Location getPickupLocation() {
    return pickupLocation;
  }

  public Location getDropLocation() {
    return dropLocation;
  }

  public ReservationStatus getStatus() {
    return status;
  }

  public void setActualPickupTime(Long actualPickupTime) {
    this.actualPickupTime = actualPickupTime;
  }

  public void setActualDropTime(Long actualDropTime) {
    this.actualDropTime = actualDropTime;
  }

  public void confirmReservation() {
    this.status = ReservationStatus.SCHEDULED;
  }
}
