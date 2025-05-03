package design_problems.food_delivery_system;

import design_problems.food_delivery_system.domain.*;
import design_problems.food_delivery_system.event.OrderEventPublisher;
import design_problems.food_delivery_system.service.*;
import design_problems.food_delivery_system.strategy.DeliveryPartnerAssignmentStrategy;
import design_problems.food_delivery_system.strategy.FirstAvailableDeliveryPartnerAssignmentStrategy;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class FoodDeliveryService {
    CustomerService customerService;
    DeliveryPartnerService deliveryPartnerService;
    RestaurantService restaurantService;
    OrderService orderService;
    NotificationService notificationService;
    Map<String, Customer> customers;
    DeliveryPartnerAssignmentStrategy deliveryPartnerAssignmentStrategy;
    OrderEventPublisher orderEventPublisher;

    FoodDeliveryService() {
        this(new FirstAvailableDeliveryPartnerAssignmentStrategy());
    }

    FoodDeliveryService(DeliveryPartnerAssignmentStrategy deliveryPartnerAssignmentStrategy) {
        this.customers = new ConcurrentHashMap<>();
        this.deliveryPartnerAssignmentStrategy = deliveryPartnerAssignmentStrategy;
        this.orderEventPublisher = new OrderEventPublisher();
        this.customerService = new CustomerService(orderEventPublisher);
        this.deliveryPartnerService = new DeliveryPartnerService(deliveryPartnerAssignmentStrategy, orderEventPublisher);
        this.restaurantService = new RestaurantService(orderEventPublisher);
        this.orderService = new OrderService(orderEventPublisher);
        this.notificationService = new NotificationService(orderEventPublisher);
    }

    public void setDeliveryPartnerAssignmentStrategy(DeliveryPartnerAssignmentStrategy deliveryPartnerAssignmentStrategy) {
        deliveryPartnerService.setStrategy(deliveryPartnerAssignmentStrategy);
    }

    public void addCustomer(Customer customer) {
        customers.put(customer.getId(), customer);
    }

    public void addDeliveryPartner(DeliveryPartner deliveryPartner) {
        deliveryPartnerService.addDeliveryPartner(deliveryPartner);
    }

    public void addRestaurant(Restaurant restaurant) {
        restaurantService.addRestaurant(restaurant);
    }

    public void addMenuItem(String restaurantId, MenuItem menuItem) {
        restaurantService.addMenuItem(restaurantId, menuItem);
    }

    public void createOrder(Order order) {
        orderService.createOrder(order);
    }
}
