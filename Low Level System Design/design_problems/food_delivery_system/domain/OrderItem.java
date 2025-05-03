package design_problems.food_delivery_system.domain;

public class OrderItem {
    private String id;
    private MenuItem menuItem;
    private int quantity;

    public OrderItem(String id, MenuItem menuItem, int quantity) {
        this.id = id;
        this.menuItem = menuItem;
        this.quantity = quantity;
    }
}
