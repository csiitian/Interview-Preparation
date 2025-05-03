package design_problems.food_delivery_system.service;

import design_problems.food_delivery_system.event.OrderEvent;
import design_problems.food_delivery_system.event.OrderEventListener;
import design_problems.food_delivery_system.event.OrderEventPublisher;

public class CustomerService implements OrderEventListener {
    OrderEventPublisher orderEventPublisher;

    public CustomerService(OrderEventPublisher orderEventPublisher) {
        this.orderEventPublisher = orderEventPublisher;
    }

    @Override
    public void onOrderEvent(OrderEvent orderEvent) {

    }
}
