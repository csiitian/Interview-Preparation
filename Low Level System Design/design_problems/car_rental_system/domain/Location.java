package design_problems.car_rental_system.domain;

public class Location {
  Long locationId;
  String name;
  String address;
  String city;
  String state;
  String zipCode;
  String country;
  double latitude;
  double longitude;

  public Location(Long locationId, String name, String address, String city, String state, String zipCode, String country, double latitude, double longitude) {
    this.locationId = locationId;
    this.name = name;
    this.address = address;
    this.city = city;
    this.state = state;
    this.zipCode = zipCode;
    this.country = country;
    this.latitude = latitude;
    this.longitude = longitude;
  }

  public Location(Long locationId, String name) {
    this.locationId = locationId;
    this.name = name;
  }
}
