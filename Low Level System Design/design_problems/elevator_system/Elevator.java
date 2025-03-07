package design_problems.elevator_system;

import java.util.HashSet;
import java.util.List;
import java.util.Set;

public class Elevator {
    int id;
    public Set<Integer> internalSelectedFloors;
    public Set<Integer> externalSelectedFloors;
    List<Floor> floors;
    Set<Integer> restrictedFloors;
    Direction direction;
    DoorStatus doorStatus;
    int currentFloor;

    Elevator(int id, List<Floor> floorList) {
        this.id = id;
        floors = floorList;
        internalSelectedFloors = new HashSet<>();
        externalSelectedFloors = new HashSet<>();
        restrictedFloors = new HashSet<>();
        direction = Direction.IDLE;
        doorStatus = DoorStatus.CLOSED;
        currentFloor = 0;
    }

    public int getId() {
        return id;
    }
}
