package design_problems.food_delivery_system.domain;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class Customer {
    private String id;
    private String name;
    private Location location;
    private Map<String, Order> orders;

    public Customer(String id, String name, Location location) {
        this.id = id;
        this.name = name;
        this.location = location;
        this.orders = new ConcurrentHashMap<>();
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

    public Map<String, Order> getOrders() {
        return orders;
    }
}
