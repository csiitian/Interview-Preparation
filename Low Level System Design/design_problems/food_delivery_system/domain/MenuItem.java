package design_problems.food_delivery_system.domain;

public class MenuItem {
    private String id;
    private String name;
    private String description;
    private double price;
    private boolean available;

    public MenuItem(String id, String name, String description, double price, boolean available) {
        this.id = id;
        this.name = name;
        this.description = description;
        this.price = price;
        this.available = available;
    }
}
