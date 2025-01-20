package design_problems.hotel_management;

import java.util.ArrayList;
import java.util.List;

public class HotelManager {
  private List<Hotel> hotels;
  private List<User> users;

  HotelManager() {
    hotels = new ArrayList<>();
    users = new ArrayList<>();
  }

  public void addHotel(Hotel hotel) {
    hotels.add(hotel);
  }

  public void addUser(User user) {
    users.add(user);
  }

  public void removeHotel(Hotel hotel) {
    hotels.remove(hotel);
  }

  public void removeUser(User user) {
    users.remove(user);
  }

  public List<Hotel> searchHotel(String name) {
    List<Hotel> result = new ArrayList<>();
    for (Hotel hotel : hotels) {
      if (hotel.getName().equals(name)) {
        result.add(hotel);
      }
    }
    return result;
  }

  public void addRoom(Hotel hotel, Room room) {
    hotel.addRoom(room);
  }

  public void removeRoom(Hotel hotel, Room room) {
    hotel.removeRoom(room);
  }

  public boolean reserveRooms(User user, Hotel hotel, List<Room> rooms, long startTime, long endTime) {
    return hotel.reserveRooms(user, rooms, startTime, endTime);
  }

  public boolean checkIn(User user, Hotel hotel, List<Reservation> reservations) {
    return hotel.checkIn(user, reservations);
  }

  public boolean checkOut(User user, Hotel hotel, List<Reservation> reservations) {
    return hotel.checkOut(user, reservations);
  }

  public void markRoomAvailable(Hotel hotel, Room room) {
    hotel.markRoomAvailable(room);
  }
}
