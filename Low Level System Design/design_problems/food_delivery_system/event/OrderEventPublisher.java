package design_problems.food_delivery_system.event;

import design_problems.food_delivery_system.domain.OrderStatus;

import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

public class OrderEventPublisher {
    Map<OrderStatus, List<OrderEventListener>> listeners = new ConcurrentHashMap<>();

    public void subscribe(OrderEventListener orderEventListener, OrderStatus orderStatus) {
        listeners.computeIfAbsent(orderStatus, k -> new CopyOnWriteArrayList<>()).add(orderEventListener);
    }

    public void publish(OrderEvent orderEvent) {
        OrderStatus orderStatus = orderEvent.getOrder().getStatus();
        if (orderStatus == null) return;
        List<OrderEventListener> orderEventListeners = listeners.get(orderStatus);
        for (OrderEventListener orderEventListener : orderEventListeners) {
            CompletableFuture.runAsync(() -> orderEventListener.onOrderEvent(orderEvent));
        }
    }
}
