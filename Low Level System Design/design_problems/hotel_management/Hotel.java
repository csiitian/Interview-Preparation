package design_problems.hotel_management;

import java.util.ArrayList;
import java.util.List;

public class Hotel {
  private String name;
  private String address;
  private List<Room> rooms;
  private List<Reservation> reservations;

  public Hotel(String name, String address, List<Room> rooms) {
    this.name = name;
    this.address = address;
    this.rooms = rooms;
  }

  public void addRoom(Room room) {
    rooms.add(room);
  }

  public void removeRoom(Room room) {
    rooms.remove(room);
  }

  public String getName() {
    return name;
  }

  public List<Room> getRooms() {
    return rooms;
  }

  public boolean reserveRooms(User user, List<Room> rooms, long startTime, long endTime) {
    // check rooms are available or not
    List<Reservation> userReservations = new ArrayList<>();
    for (Room room: rooms) {
      if (room.getStatus() == RoomStatus.AVAILABLE) {
        Reservation reservation = null; // new Reservation(user, room, startTime, endTime);
        userReservations.add(reservation);
      } else {
        return false;
      }
    }

    reservations.addAll(userReservations);
    return true;
  }

  public boolean checkIn(User user, List<Reservation> reservations) {
    // check in user to rooms
    for (Reservation reservation: reservations) {
      if (reservation.getStatus() == ReservationStatus.CONFIRMED) {
        reservation.setStatus(ReservationStatus.CHECKED_IN);
        reservation.getRoom().setStatus(RoomStatus.OCCUPIED);
      } else {
        return false;
      }
    }
    return true;
  }

  public boolean checkOut(User user, List<Reservation> reservations) {
    // check out user from rooms
    for (Reservation reservation: reservations) {
      if (reservation.getStatus() == ReservationStatus.CHECKED_IN) {
        reservation.setStatus(ReservationStatus.CHECKED_OUT);
        reservation.getRoom().setStatus(RoomStatus.BEING_SERVICED);
      } else {
        return false;
      }
    }
    return true;
  }

  public void markRoomAvailable(Room room) {
    room.setStatus(RoomStatus.AVAILABLE);
  }
}
