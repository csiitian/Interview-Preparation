package design_problems.car_rental_system.domain;

import java.util.ArrayList;
import java.util.List;

public class VehicleStore {
  Long storeId;
  String storeName;
  String address;
  String city;
  String state;
  String zip;
  String country;
  List<Vehicle> vehicles;
  Boolean isOpen;

  public VehicleStore(Long storeId, String storeName, String address, String city, String state, String zip, String country, Boolean isOpen) {
    this.storeId = storeId;
    this.storeName = storeName;
    this.address = address;
    this.city = city;
    this.state = state;
    this.zip = zip;
    this.country = country;
    this.vehicles = new ArrayList<>();
    this.isOpen = isOpen;
  }

  public void addVehicles(List<Vehicle> vehicle) {
    this.vehicles.addAll(vehicle);
  }
}
