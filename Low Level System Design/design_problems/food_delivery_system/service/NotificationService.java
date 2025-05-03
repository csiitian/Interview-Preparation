package design_problems.food_delivery_system.service;

import design_problems.food_delivery_system.domain.OrderStatus;
import design_problems.food_delivery_system.event.OrderEventPublisher;
import design_problems.food_delivery_system.event.OrderEvent;
import design_problems.food_delivery_system.event.OrderEventListener;

public class NotificationService implements OrderEventListener {

    public NotificationService(OrderEventPublisher orderEventPublisher) {
        orderEventPublisher.subscribe(this, OrderStatus.PLACED);
        orderEventPublisher.subscribe(this, OrderStatus.PREPARING);
        orderEventPublisher.subscribe(this, OrderStatus.READY);
        orderEventPublisher.subscribe(this, OrderStatus.ON_THE_WAY);
        orderEventPublisher.subscribe(this, OrderStatus.DELIVERED);
    }

    @Override
    public void onOrderEvent(OrderEvent orderEvent) {
        System.out.println("Notification Service: Order event received." + orderEvent);
    }
}
