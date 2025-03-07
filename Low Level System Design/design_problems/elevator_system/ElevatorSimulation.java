package design_problems.elevator_system;

public class ElevatorSimulation implements Runnable {

    ElevatorSystem elevatorSystem;

    ElevatorSimulation(ElevatorSystem elevatorSystem) {
        this.elevatorSystem = elevatorSystem;
    }

    @Override
    public void run() {
        while (true) {
            for (Elevator elevator : elevatorSystem.getElevatorList()) {
                if (elevator.externalSelectedFloors.isEmpty() && elevator.internalSelectedFloors.isEmpty()) {
                    elevator.direction = Direction.IDLE;
                } else if (elevator.direction == Direction.UP) {
                    if (elevator.currentFloor == elevator.floors.size() - 1) {
                        elevator.direction = Direction.DOWN;
                    }
                    elevator.currentFloor += 1;
                } else if (elevator.direction == Direction.DOWN) {
                    if (elevator.currentFloor == 0) {
                        elevator.direction = Direction.UP;
                    }
                }
                System.out.println(elevator.getId() + " -> " + elevator.direction + " " + elevator.currentFloor);
            }
            System.out.println("---------------------------------------");
            try {
                Thread.sleep(5000);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
    }
}
