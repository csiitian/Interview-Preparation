package design_problems.food_delivery_system.strategy;

import design_problems.food_delivery_system.domain.DeliveryPartner;
import design_problems.food_delivery_system.domain.DeliveryPartnerStatus;
import design_problems.food_delivery_system.domain.Order;

import java.util.List;
import java.util.Optional;

public class FirstAvailableDeliveryPartnerAssignmentStrategy implements DeliveryPartnerAssignmentStrategy {

    @Override
    public Optional<DeliveryPartner> assign(List<DeliveryPartner> deliveryPartnerList, Order order) {
        return deliveryPartnerList.stream()
                .filter(deliveryPartner -> deliveryPartner.getStatus() == DeliveryPartnerStatus.FREE)
                .findFirst();
    }
}
