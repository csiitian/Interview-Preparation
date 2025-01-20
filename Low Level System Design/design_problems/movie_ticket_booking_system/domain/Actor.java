package design_problems.movie_ticket_booking_system.domain;

import java.util.UUID;

public class Actor {
    String actorId;
    String name;
    String bio;

    public Actor(String name) {
        this.actorId = UUID.randomUUID().toString();
        this.name = name;
    }
}
