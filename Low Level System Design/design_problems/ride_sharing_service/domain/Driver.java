package design_problems.ride_sharing_service.domain;

public class Driver {
    private String id;
    private String name;
    private String email;
    private Location location;
    private boolean available;

    public String getId() {
        return id;
    }

    public Location getLocation() {
        return location;
    }

    public boolean isAvailable() {
        return available;
    }
}
