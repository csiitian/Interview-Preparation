package design_problems.food_delivery_system.event;

public interface OrderEventListener {
    void onOrderEvent(OrderEvent orderEvent);
}
