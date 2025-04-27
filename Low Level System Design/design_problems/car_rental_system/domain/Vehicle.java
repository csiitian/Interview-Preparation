package design_problems.car_rental_system.domain;

public class Vehicle {
  Long storeId;
  Long vehicleId;
  String vehicleNo;
  String model;
  Integer noOfSeats;
  VehicleType type;
  VehicleStatus status;
  Double hourlyRate;
  Long distanceTravelled;
  Double avgMileage;
  VehicleCatalog catalog;

  public Vehicle(Long storeId, Long vehicleId, String vehicleNo, String model, Integer noOfSeats, VehicleType type, VehicleStatus status, Double hourlyRate, VehicleCatalog catalog) {
    this.storeId = storeId;
    this.vehicleId = vehicleId;
    this.vehicleNo = vehicleNo;
    this.model = model;
    this.noOfSeats = noOfSeats;
    this.type = type;
    this.status = status;
    this.hourlyRate = hourlyRate;
    this.catalog = catalog;
  }

  public Long getStoreId() {
    return storeId;
  }

  public Long getVehicleId() {
    return vehicleId;
  }

  public String getVehicleNo() {
    return vehicleNo;
  }

  public Integer getNoOfSeats() {
    return noOfSeats;
  }

  public String getModel() {
    return model;
  }

  public VehicleType getType() {
    return type;
  }

  public VehicleStatus getStatus() {
    return status;
  }

  public Double getHourlyRate() {
    return hourlyRate;
  }

  public VehicleCatalog getCatalog() {
    return catalog;
  }

  public void setStatus(VehicleStatus status) {
    this.status = status;
  }

  public synchronized boolean isAvailable() {
    return status == VehicleStatus.AVAILABLE;
  }
}
