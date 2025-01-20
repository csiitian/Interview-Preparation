package design_problems.movie_ticket_booking_system.domain;

import java.util.UUID;

public class Theatre {
    String theatreId;
    String name;
    City city;

    public Theatre(String name, City city) {
        this.theatreId = UUID.randomUUID().toString();
        this.name = name;
        this.city = city;
    }

    public String getTheatreId() {
        return theatreId;
    }

    public void setTheatreId(String theatreId) {
        this.theatreId = theatreId;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public City getLocation() {
        return city;
    }

    public void setLocation(City city) {
        this.city = city;
    }
}
