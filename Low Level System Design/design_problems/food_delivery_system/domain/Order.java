package design_problems.food_delivery_system.domain;

import java.time.LocalDateTime;
import java.util.List;

public class Order implements IPrototype {
    private String id;
    private String restaurantId;
    private List<OrderItem> orderItems;
    private double amount;
    private OrderStatus status;
    private Location location;
    private String address;
    Payment payment;
    private LocalDateTime createdAt;
    private LocalDateTime deliveredAt;

    public Order(String id, String restaurantId, List<OrderItem> orderItems, double amount, OrderStatus orderStatus, Location location, String address) {
        this.id = id;
        this.restaurantId = restaurantId;
        this.orderItems = orderItems;
        this.amount = amount;
        this.status = orderStatus;
        this.location = location;
        this.address = address;
    }

    public Location getLocation() {
        return location;
    }

    public OrderStatus getStatus() {
        return status;
    }

    public void setStatus(OrderStatus status) {
        this.status = status;
    }

    @Override
    public String toString() {
        return "Order{" +
                "id='" + id + '\'' +
                ", restaurantId='" + restaurantId + '\'' +
                ", orderItems=" + orderItems +
                ", amount=" + amount +
                ", status=" + status +
                '}';
    }

    @Override
    public Order clone() {
        try {
            return (Order) super.clone();
        } catch (CloneNotSupportedException e) {
            return null;
        }
    }
}
