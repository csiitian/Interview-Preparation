package design_problems.hotel_management;

public class Room {
  private int roomNumber;
  private int floor;
  private RoomType type;
  private RoomStatus status;

  public RoomStatus getStatus() {
    return status;
  }

  public void setStatus(RoomStatus status) {
    this.status = status;
  }
}
