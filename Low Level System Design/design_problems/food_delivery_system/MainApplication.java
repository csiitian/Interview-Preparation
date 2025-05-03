package design_problems.food_delivery_system;

import design_problems.food_delivery_system.domain.*;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;

public class MainApplication {
    public static void main(String[] args) {
        FoodDeliveryService foodDeliveryService = new FoodDeliveryService();
        Customer customer = new Customer(UUID.randomUUID().toString(), "Smith", new Location(139.12, 123));
        DeliveryPartner deliveryPartner = new DeliveryPartner(UUID.randomUUID().toString(), "John", new Location(12, 89.1), DeliveryPartnerStatus.FREE);
        List<MenuItem> menuItems = Collections.synchronizedList(new ArrayList<>());
        menuItems.add(new MenuItem(UUID.randomUUID().toString(), "Pizza", "Pizza",350, true));
        menuItems.add(new MenuItem(UUID.randomUUID().toString(), "Burger", "Burger",50, true));
        menuItems.add(new MenuItem(UUID.randomUUID().toString(), "Samosa", "Samosa",20, true));
        Restaurant restaurant = new Restaurant(UUID.randomUUID().toString(), "Alka Hotel", "Sidhi Road, Sikar", new Location(24, 124), menuItems);
        foodDeliveryService.addCustomer(customer);
        foodDeliveryService.addDeliveryPartner(deliveryPartner);
        foodDeliveryService.addRestaurant(restaurant);

        List<OrderItem> orderItems = Collections.synchronizedList(new ArrayList<>());
        orderItems.add(new OrderItem(UUID.randomUUID().toString(), menuItems.get(1), 3));
        Order order = new Order(
                UUID.randomUUID().toString(),
                restaurant.getId(),
                orderItems,
                170,
                OrderStatus.PLACED,
                customer.getLocation(),
                "Silk Road, India"
        );
        foodDeliveryService.createOrder(order);

        try {
            Thread.sleep(20000); // waiting for the task to complete because we have delay for simulation
        } catch (InterruptedException e) {
            System.out.println(e.getMessage());
        }

        System.out.println("Done.");
    }
}
