package design_problems.movie_ticket_booking_system.domain;

import java.util.List;
import java.util.UUID;

public class Ticket {
    String ticketId;
    String showId;
    List<Seat> bookedSeats;
    double price;
    TicketStatus status;

    public Ticket(String showId, List<Seat> bookedSeats, double price, TicketStatus status) {
        this.ticketId = UUID.randomUUID().toString();
        this.showId = showId;
        this.bookedSeats = bookedSeats;
        this.price = price;
        this.status = status;
    }

    public String getTicketId() {
        return ticketId;
    }

    public void setTicketId(String ticketId) {
        this.ticketId = ticketId;
    }

    public String getShowId() {
        return showId;
    }

    public void setShowId(String showId) {
        this.showId = showId;
    }

    public List<Seat> getBookedSeats() {
        return bookedSeats;
    }

    public void setBookedSeats(List<Seat> bookedSeats) {
        this.bookedSeats = bookedSeats;
    }

    public double getPrice() {
        return price;
    }

    public void setPrice(double price) {
        this.price = price;
    }

    public TicketStatus getStatus() {
        return status;
    }

    public void setStatus(TicketStatus status) {
        this.status = status;
    }

    @Override
    public String toString() {
        return "Ticket{" +
                "ticketId='" + ticketId + '\'' +
                ", showId='" + showId + '\'' +
                ", bookedSeats=" + bookedSeats +
                ", price=" + price +
                ", status=" + status +
                '}';
    }
}
