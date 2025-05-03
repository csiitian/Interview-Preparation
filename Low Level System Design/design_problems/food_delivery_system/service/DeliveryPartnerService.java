package design_problems.food_delivery_system.service;

import design_problems.food_delivery_system.domain.DeliveryPartner;
import design_problems.food_delivery_system.domain.DeliveryPartnerStatus;
import design_problems.food_delivery_system.domain.Order;
import design_problems.food_delivery_system.domain.OrderStatus;
import design_problems.food_delivery_system.event.OrderEvent;
import design_problems.food_delivery_system.event.OrderEventListener;
import design_problems.food_delivery_system.event.OrderEventPublisher;
import design_problems.food_delivery_system.strategy.DeliveryPartnerAssignmentStrategy;

import java.util.ArrayList;
import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

public class DeliveryPartnerService implements OrderEventListener {
    Map<String, DeliveryPartner> deliveryPartners;
    DeliveryPartnerAssignmentStrategy strategy;
    OrderEventPublisher orderEventPublisher;

    public DeliveryPartnerService(DeliveryPartnerAssignmentStrategy strategy, OrderEventPublisher orderEventPublisher) {
        this.deliveryPartners = new ConcurrentHashMap<>();
        this.strategy = strategy;
        this.orderEventPublisher = orderEventPublisher;
        orderEventPublisher.subscribe(this, OrderStatus.READY);
    }

    public void setStrategy(DeliveryPartnerAssignmentStrategy strategy) {
        this.strategy = strategy;
    }

    @Override
    public void onOrderEvent(OrderEvent orderEvent) {
        Order order = orderEvent.getOrder().clone();
        if (order == null) {
            System.out.println("Delivery Partner Service: Order is null");
        }
        Optional<DeliveryPartner> deliveryPartnerOptional = strategy.assign(new ArrayList<>(deliveryPartners.values()), order);
        if (deliveryPartnerOptional.isPresent()) {
            DeliveryPartner deliveryPartner = deliveryPartnerOptional.get();
            deliveryPartner.setStatus(DeliveryPartnerStatus.BUSY);
            System.out.println("Delivery Partner Service: Delivery partner assigned: " + deliveryPartner);
            System.out.println("Delivery Partner Service: Order is on the way.");
            new Thread(() -> {
                try {
                    Thread.sleep(2000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }).start();

            // update order status
            order.setStatus(OrderStatus.ON_THE_WAY);
            orderEventPublisher.publish(new OrderEvent(order));
            
            // update delivered after simulation
            new Thread(() -> {
                try {
                    Thread.sleep(5000);
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
                order.setStatus(OrderStatus.DELIVERED);
                orderEventPublisher.publish(new OrderEvent(order));
            }).start();
        } else {
            System.out.println("Delivery Partner Service: No delivery partner assigned.");
        }
    }

    public void addDeliveryPartner(DeliveryPartner deliveryPartner) {
        deliveryPartners.put(deliveryPartner.getId(), deliveryPartner);
    }
}
