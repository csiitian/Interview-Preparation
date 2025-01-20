package design_problems.movie_ticket_booking_system;

import design_problems.movie_ticket_booking_system.domain.*;
import design_problems.movie_ticket_booking_system.exception.SeatNotAvailableException;

import java.util.*;

public class MovieTicketBookingService {
    static MovieTicketBookingService instance;
    Map<String, Theatre> theatreMap;
    Map<String, Hall> hallMap;
    Map<String, Show> showMap;

    private MovieTicketBookingService() {
        theatreMap = new HashMap<>();
        hallMap = new HashMap<>();
        showMap = new HashMap<>();
    }

    public static synchronized MovieTicketBookingService getInstance() {
        if (instance == null) {
            instance = new MovieTicketBookingService();
        }
        return instance;
    }

    public void addTheatre(Theatre theatre) {
        theatreMap.put(theatre.getTheatreId(), theatre);
    }

    public void addHall(Hall hall) {
        hallMap.put(hall.getHallId(), hall);
    }

    public void addShow(Show show) {
        showMap.put(show.getShowId(), show);
    }

    public synchronized Ticket bookTicket(Show show, Set<String> seats) {
        double price = 0d;
        // check all the seats are available
        List<Seat> bookedSeats = new ArrayList<>();
        for (Seat seat: show.getAvailableSeats().values()) {
            if (seats.contains(seat.getCode())) {
                if (seat.isAvailable()) {
                    bookedSeats.add(seat);
                } else {
                    throw new SeatNotAvailableException(seat.getCode() + " is not available");
                }
            }
        }
        // now mark all the seat as reserved and calculate price
        for (Seat reservedSeat: bookedSeats) {
            reservedSeat.setStatus(SeatStatus.IN_PROGRESS);
            price += reservedSeat.getPrice();
        }
        // generate ticket
        Ticket ticket = new Ticket(show.getShowId(), bookedSeats, price, TicketStatus.PENDING);
        return ticket;
    }

    public synchronized void makePayment(Ticket ticket) {
        Payment payment = new Payment(ticket.getTicketId(), ticket.getPrice(), PaymentStatus.PENDING);
        payment.setStatus(PaymentStatus.PAID);
        // mark ticket and seat as reserved
        ticket.setStatus(TicketStatus.RESERVED);
        for (Seat seat: ticket.getBookedSeats()) {
            seat.setStatus(SeatStatus.BOOKED);
        }
    }
}
