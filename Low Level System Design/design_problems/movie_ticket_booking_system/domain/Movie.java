package design_problems.movie_ticket_booking_system.domain;

import java.time.Duration;
import java.util.List;

public class Movie {
    String movieId;
    String name;
    Duration duration; // minutes
    String genre;
    double rating;
    String posterImage;
    List<Actor> actors;

    public Movie(String name, Duration duration, String genre, double rating, List<Actor> actors) {
        this.name = name;
        this.duration = duration;
        this.genre = genre;
        this.rating = rating;
        this.actors = actors;
    }
}
