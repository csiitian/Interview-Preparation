package design_problems.food_delivery_system.service;

import design_problems.food_delivery_system.domain.Order;
import design_problems.food_delivery_system.event.OrderEvent;
import design_problems.food_delivery_system.event.OrderEventListener;
import design_problems.food_delivery_system.event.OrderEventPublisher;

public class OrderService implements OrderEventListener {
    OrderEventPublisher orderEventPublisher;

    public OrderService(OrderEventPublisher orderEventPublisher) {
        this.orderEventPublisher = orderEventPublisher;
    }

    @Override
    public void onOrderEvent(OrderEvent orderEvent) {

    }

    public void createOrder(Order order) {
        orderEventPublisher.publish(new OrderEvent(order)); // notify restaurants
    }
}
