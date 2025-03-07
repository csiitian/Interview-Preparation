package design_problems.elevator_system;

import java.util.List;
import java.util.Optional;

public interface ElevatorAssignmentStrategy {
    Optional<Elevator> findBestElevator(Request request, List<Elevator> elevatorList);
}
