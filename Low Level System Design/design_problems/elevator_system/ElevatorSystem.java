package design_problems.elevator_system;

import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;

public class ElevatorSystem {
    private final List<Elevator> elevatorList;
    private final List<Floor> floorList;
    private final Queue<Request> requestQueue;
    private final Set<Request> requestSet; // avoid duplicate addition request to queue
    private ElevatorAssignmentStrategy elevatorAssignmentStrategy;

    ElevatorSystem(int elevators, int floors) {
        elevatorList = new ArrayList<>();
        floorList = new ArrayList<>();
        for (int i = 0; i < floors; i++) {
            floorList.add(new Floor(i + 1));
        }
        for (int i = 0; i < elevators; i++) {
            elevatorList.add(new Elevator((i + 1), floorList));
        }
        elevatorAssignmentStrategy = new NearestElevatorAssignmentStrategy();
        requestQueue = new ConcurrentLinkedQueue<>();
        requestSet = ConcurrentHashMap.newKeySet();
    }

    public void setElevatorAssignmentStrategy(ElevatorAssignmentStrategy elevatorAssignmentStrategy) {
        this.elevatorAssignmentStrategy = elevatorAssignmentStrategy;
    }

    public void processExternalRequest(Request request) {
        Optional<Elevator> elevatorOptional = elevatorAssignmentStrategy.findBestElevator(request, elevatorList);

        if (elevatorOptional.isPresent()) {
            Elevator elevator = elevatorOptional.get();
            elevator.externalSelectedFloors.add(request.floor);
            if (elevator.direction == Direction.IDLE) {
              elevator.direction = request.direction;
            }
        } else {
            if (!requestSet.contains(request)) {
                requestQueue.add(request);
                requestSet.add(request);
            }
        }
    }

    public void processInternalRequest(Elevator elevator, int floor) {
        elevator.internalSelectedFloors.add(floor);
    }

  public List<Elevator> getElevatorList() {
    return elevatorList;
  }
}
