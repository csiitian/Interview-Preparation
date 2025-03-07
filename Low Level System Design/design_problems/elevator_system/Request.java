package design_problems.elevator_system;

public class Request {
    int floor;
    Direction direction; // UP or DOWN

    public Request(int floor, Direction direction) {
        this.floor = floor;
        this.direction = direction;
    }
}
