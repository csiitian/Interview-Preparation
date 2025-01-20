package solid_principles.LiskovSubstitutePrinciple.Solution;

import java.util.ArrayList;
import java.util.List;

public class MainApplication {

  public static void main(String[] args) {
    List<EngineVehicle> engineVehicleList = new ArrayList<>();
    engineVehicleList.add(new Car());
    engineVehicleList.add(new Bike());

    for (EngineVehicle engineVehicle: engineVehicleList) {
      System.out.println(engineVehicle.hasEngine());
    }

    List<Vehicle> vehicleList = new ArrayList<>();
    vehicleList.add(new Car());
    vehicleList.add(new Bike());
    vehicleList.add(new Bicycle());

    for (Vehicle vehicle: vehicleList) {
      System.out.println(vehicle.noOfWheels());
    }
  }
}
