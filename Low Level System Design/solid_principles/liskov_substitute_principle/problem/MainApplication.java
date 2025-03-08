package solid_principles.liskov_substitute_principle.problem;

import java.util.ArrayList;
import java.util.List;

public class MainApplication {

  public static void main(String[] args) {
    List<Vehicle> vehicleList = new ArrayList<>();
    vehicleList.add(new Car());
    vehicleList.add(new Bike());
    vehicleList.add(new Bicycle());

    for (Vehicle vehicle: vehicleList) {
      System.out.println(vehicle.hasEngine());
    }
  }
}
