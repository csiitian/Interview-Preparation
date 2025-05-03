package design_problems.food_delivery_system.service;

import design_problems.food_delivery_system.domain.MenuItem;
import design_problems.food_delivery_system.domain.Order;
import design_problems.food_delivery_system.domain.OrderStatus;
import design_problems.food_delivery_system.domain.Restaurant;
import design_problems.food_delivery_system.event.OrderEventPublisher;
import design_problems.food_delivery_system.event.OrderEvent;
import design_problems.food_delivery_system.event.OrderEventListener;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class RestaurantService implements OrderEventListener {
    OrderEventPublisher orderEventPublisher;
    Map<String, Restaurant> restaurants;

    public RestaurantService(OrderEventPublisher orderEventPublisher) {
        this.orderEventPublisher = orderEventPublisher;
        restaurants = new ConcurrentHashMap<>();
        orderEventPublisher.subscribe(this, OrderStatus.PLACED);
    }

    @Override
    public void onOrderEvent(OrderEvent orderEvent) {
        new Thread(() -> {
            Order order = orderEvent.getOrder();
            System.out.println("Restaurant Service: Preparing order - " + order);
            try {
                Thread.sleep(5000);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            order.setStatus(OrderStatus.READY);
            orderEventPublisher.publish(new OrderEvent(order));
        }).start();
    }

    public void addRestaurant(Restaurant restaurant) {
        restaurants.put(restaurant.getId(), restaurant);
    }

    public void addMenuItem(String restaurantId, MenuItem menuItem) {
        Restaurant restaurant = restaurants.get(restaurantId);
        if (restaurant == null) {
            System.out.println("Restaurant Service: Restaurant " + restaurantId + " not found");
            return;
        }
        restaurant.addMenuItem(menuItem);
    }
}
