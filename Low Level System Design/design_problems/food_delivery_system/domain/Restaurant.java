package design_problems.food_delivery_system.domain;

import java.util.ArrayList;
import java.util.List;

public class Restaurant {
    private String id;
    private String name;
    private String address;
    private Location location;
    private List<MenuItem> menuItems;

    public Restaurant(String id, String name, String address, Location location, List<MenuItem> menuItems) {
        this.id = id;
        this.name = name;
        this.address = address;
        this.location = location;
        this.menuItems = menuItems;
    }

    public String getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public String getAddress() {
        return address;
    }

    public Location getLocation() {
        return location;
    }

    public List<MenuItem> getMenuItems() {
        return menuItems;
    }

    public void addMenuItem(MenuItem menuItem) {
        if (menuItems == null) menuItems = new ArrayList<>();
        menuItems.add(menuItem);
    }
}
