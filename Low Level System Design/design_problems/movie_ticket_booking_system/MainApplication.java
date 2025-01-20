package design_problems.movie_ticket_booking_system;

import design_problems.movie_ticket_booking_system.domain.*;

import java.time.Duration;
import java.time.LocalDateTime;
import java.util.*;

public class MainApplication {
    public static void main(String[] args) {
        MovieTicketBookingService movieTicketBookingService = MovieTicketBookingService.getInstance();

        Theatre theatre = new Theatre(
                "Sangam",
                new City("Jhunjhunu", "Rajasthan")
        );
        movieTicketBookingService.addTheatre(theatre);

        Hall hall = new Hall(
                theatre.getTheatreId(),
                "AA01"
        );
        movieTicketBookingService.addHall(hall);

        Movie movie = new Movie(
                "Pushpa 2",
                Duration.ofMinutes(180),
                "Comedy",
                4.5,
                List.of(new Actor("Allu Arjuna"))
        );

        Map<String, Seat> seatMap = new HashMap<>();
        seatMap.put("H01", new Seat("H01", SeatType.HIGH, 120, SeatStatus.AVAILABLE));
        seatMap.put("M01", new Seat("M01", SeatType.MIDDLE, 90, SeatStatus.AVAILABLE));
        seatMap.put("M02", new Seat("M02", SeatType.MIDDLE, 90, SeatStatus.AVAILABLE));
        seatMap.put("L01", new Seat("L01", SeatType.LOWER, 60, SeatStatus.AVAILABLE));
        seatMap.put("L02", new Seat("L0", SeatType.LOWER, 60, SeatStatus.AVAILABLE));

        Show show = new Show(
                hall.getHallId(),
                movie,
                LocalDateTime.of(2025, 1, 1, 12, 0, 0),
                LocalDateTime.of(2025, 1, 1, 15, 0, 0),
                seatMap
        );
        movieTicketBookingService.addShow(show);

        try {
            Ticket ticket = movieTicketBookingService.bookTicket(show, new HashSet<>(Arrays.asList("H01", "L01")));
            System.out.println(ticket);
            movieTicketBookingService.makePayment(ticket);
        } catch (RuntimeException exception) {
            System.out.println(exception.getMessage());
        }

        try {
            Ticket ticket2 = movieTicketBookingService.bookTicket(show, new HashSet<>(Arrays.asList("H01", "L01")));
            System.out.println(ticket2);
            movieTicketBookingService.makePayment(ticket2);
        } catch (RuntimeException exception) {
            System.out.println(exception.getMessage());
        }
    }
}
