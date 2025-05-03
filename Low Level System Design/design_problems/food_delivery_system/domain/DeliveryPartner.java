package design_problems.food_delivery_system.domain;

public class DeliveryPartner {
    private String id;
    private String name;
    private Location location;
    private DeliveryPartnerStatus status;

    public DeliveryPartner(String id, String name, Location location, DeliveryPartnerStatus status) {
        this.id = id;
        this.name = name;
        this.location = location;
        this.status = status;
    }

    public String getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public Location getLocation() {
        return location;
    }

    public DeliveryPartnerStatus getStatus() {
        return status;
    }

    public void setStatus(DeliveryPartnerStatus status) {
        this.status = status;
    }
}
