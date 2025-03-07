package design_problems.elevator_system;

public class MainApplication {
    public static void main(String[] args) {
        ElevatorSystem elevatorSystem = new ElevatorSystem(2, 6);
        ElevatorSimulation simulation = new ElevatorSimulation(elevatorSystem);
        Thread thread = new Thread(simulation);
        thread.start();

        elevatorSystem.processExternalRequest(new Request(1, Direction.UP));
    }
}
