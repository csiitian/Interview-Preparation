package design_problems.food_delivery_system.strategy;

import design_problems.food_delivery_system.domain.DeliveryPartner;
import design_problems.food_delivery_system.domain.Order;

import java.util.List;
import java.util.Optional;

public interface DeliveryPartnerAssignmentStrategy {
    Optional<DeliveryPartner> assign(List<DeliveryPartner> deliveryPartnerList, Order order);
}
