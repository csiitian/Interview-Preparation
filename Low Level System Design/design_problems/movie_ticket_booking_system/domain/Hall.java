package design_problems.movie_ticket_booking_system.domain;

import java.util.UUID;

public class Hall {
    String hallId;
    String theatreId;
    String code;

    public Hall(String theatreId, String code) {
        this.hallId = UUID.randomUUID().toString();
        this.theatreId = theatreId;
        this.code = code;
    }

    public String getHallId() {
        return hallId;
    }

    public void setHallId(String hallId) {
        this.hallId = hallId;
    }

    public String getTheatreId() {
        return theatreId;
    }

    public void setTheatreId(String theatreId) {
        this.theatreId = theatreId;
    }

    public String getCode() {
        return code;
    }

    public void setCode(String code) {
        this.code = code;
    }
}
