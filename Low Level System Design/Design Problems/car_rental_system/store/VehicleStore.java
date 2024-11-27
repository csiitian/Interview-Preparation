package system_design.car_rental_system.store;

import java.util.ArrayList;
import java.util.List;
import system_design.car_rental_system.vehicle.Vehicle;

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
