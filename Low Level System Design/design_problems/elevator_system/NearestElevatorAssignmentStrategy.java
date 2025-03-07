package design_problems.elevator_system;

import java.util.Comparator;
import java.util.List;
import java.util.Optional;

public class NearestElevatorAssignmentStrategy implements ElevatorAssignmentStrategy {
    @Override
    public Optional<Elevator> findBestElevator(Request request, List<Elevator> elevatorList) {
        return elevatorList.stream()
                .filter(elevator -> elevator.direction == request.direction || elevator.direction == Direction.IDLE)
                .filter(elevator -> elevator.currentFloor <= request.floor)
                .sorted((e1, e2) -> {
                    if (e1.direction == Direction.IDLE) return 1;
                    else if (e2.direction == Direction.IDLE) return -1;
                    else return 0;
                })
                .min(Comparator.comparingInt(e -> e.currentFloor));
    }
}
