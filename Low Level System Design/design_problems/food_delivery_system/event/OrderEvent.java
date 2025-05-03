package design_problems.food_delivery_system.event;

import design_problems.food_delivery_system.domain.Order;

public class OrderEvent {
    private Order order;

    public OrderEvent(Order order) {
        this.order = order.clone();
    }

    public Order getOrder() {
        return order;
    }

    @Override
    public String toString() {
        return "OrderEvent{" +
                "order=" + order +
                '}';
    }
}
