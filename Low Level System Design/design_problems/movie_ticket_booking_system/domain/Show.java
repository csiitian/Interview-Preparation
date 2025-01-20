package design_problems.movie_ticket_booking_system.domain;

import java.time.LocalDateTime;
import java.util.*;

public class Show {
    String showId;
    String hallId;
    Movie movie;
    LocalDateTime startTime;
    LocalDateTime endTime;
    Map<String, Seat> availableSeats;

    public Show(String hallId, Movie movie, LocalDateTime startTime, LocalDateTime endTime,
                Map<String, Seat> availableSeats) {
        this.showId = UUID.randomUUID().toString();
        this.hallId = hallId;
        this.movie = movie;
        this.startTime = startTime;
        this.endTime = endTime;
        this.availableSeats = availableSeats;
    }

    void addSeat(String seatId, Seat seat) {
        if (availableSeats == null) availableSeats = new HashMap<>();
        availableSeats.put(seatId, seat);
    }

    public String getShowId() {
        return showId;
    }

    public void setShowId(String showId) {
        this.showId = showId;
    }

    public String getHallId() {
        return hallId;
    }

    public void setHallId(String hallId) {
        this.hallId = hallId;
    }

    public Movie getMovie() {
        return movie;
    }

    public void setMovie(Movie movie) {
        this.movie = movie;
    }

    public LocalDateTime getStartTime() {
        return startTime;
    }

    public void setStartTime(LocalDateTime startTime) {
        this.startTime = startTime;
    }

    public LocalDateTime getEndTime() {
        return endTime;
    }

    public void setEndTime(LocalDateTime endTime) {
        this.endTime = endTime;
    }

    public Map<String, Seat> getAvailableSeats() {
        return availableSeats;
    }

    public void setAvailableSeats(Map<String, Seat> availableSeats) {
        this.availableSeats = availableSeats;
    }
}
