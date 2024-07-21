package system_design.elevator_system;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Elevator {
  List<Floor> floors;
  public Set<Integer> selectedFloors;
  Set<Integer> restrictedFloors;
  Direction direction;
  DoorStatus doorStatus;
  int currentFloor;

  Elevator(List<Floor> floorList) {
    floors = floorList;
    selectedFloors = new HashSet<>();
    restrictedFloors = new HashSet<>();
    direction = Direction.IDLE;
    doorStatus = DoorStatus.CLOSED;
    currentFloor = 0;
  }
}
