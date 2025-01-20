package design_problems.hotel_management;

public class Reservation {
  private String userId;
  private Hotel hotel;
  private Room room;
  private long startTime;
  private long endTime;
  private ReservationStatus status;

  public ReservationStatus getStatus() {
    return status;
  }

  public Room getRoom() {
    return room;
  }

  public void setStatus(ReservationStatus status) {
    this.status = status;
  }
}
