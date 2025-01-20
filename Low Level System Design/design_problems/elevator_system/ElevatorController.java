package design_problems.elevator_system;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Optional;
import java.util.Queue;
import java.util.Set;

public class ElevatorController {
  List<Elevator> elevatorList;
  List<Floor> floorList;
  Queue<Request> requestQueue;
  Set<Request> requestSet; // avoid duplicate addition request to queue

  ElevatorController(int elevators, int floors) {
    elevatorList = new ArrayList<>();
    floorList = new ArrayList<>();
    for(int i=0;i<floors;i++) {
      floorList.add(new Floor(i+1));
    }
    for(int i=0;i<elevators;i++) {
      elevatorList.add(new Elevator(floorList));
    }
  }

  public void processExternalRequest(Request request) {
    if(request.direction == Direction.UP) {
      Optional<Elevator> elevatorOptional = elevatorList.stream().filter(elevator -> elevator.direction == Direction.UP)
          .filter(elevator -> elevator.currentFloor >= request.floor)
          .sorted((e1, e2) -> e2.currentFloor - e1.currentFloor)
          .findFirst();

      if(elevatorOptional.isPresent()) {
        Elevator elevator = elevatorOptional.get();
        elevator.selectedFloors.add(request.floor);
      } else {
        if(!requestSet.contains(request)) {
          requestQueue.add(request);
        }
      }
    } else {
      Optional<Elevator> elevatorOptional = elevatorList.stream().filter(elevator -> elevator.direction == Direction.DOWN)
          .filter(elevator -> elevator.currentFloor <= request.floor)
          .sorted(Comparator.comparingInt(e -> e.currentFloor))
          .findFirst();

      if(elevatorOptional.isPresent()) {
        Elevator elevator = elevatorOptional.get();
        elevator.selectedFloors.add(request.floor);
      } else {
        if(!requestSet.contains(request)) {
          requestQueue.add(request);
        }
      }
    }
  }

  public void processInternalRequest(Elevator elevator, int floor) {
    elevator.selectedFloors.add(floor);
  }
}
