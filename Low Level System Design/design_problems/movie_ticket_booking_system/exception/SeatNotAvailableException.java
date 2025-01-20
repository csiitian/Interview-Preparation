package design_problems.movie_ticket_booking_system.exception;

public class SeatNotAvailableException extends RuntimeException {
    private String message;

    public SeatNotAvailableException(String message) {
        super(message);
        this.message = message;
    }
}
