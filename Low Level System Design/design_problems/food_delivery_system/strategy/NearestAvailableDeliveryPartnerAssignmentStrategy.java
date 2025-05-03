package design_problems.food_delivery_system.strategy;

import design_problems.food_delivery_system.domain.DeliveryPartner;
import design_problems.food_delivery_system.domain.DeliveryPartnerStatus;
import design_problems.food_delivery_system.domain.Order;

import java.util.List;
import java.util.Optional;

import static design_problems.food_delivery_system.utils.Utils.getDistance;

public class NearestAvailableDeliveryPartnerAssignmentStrategy implements DeliveryPartnerAssignmentStrategy {

    @Override
    public Optional<DeliveryPartner> assign(List<DeliveryPartner> deliveryPartnerList, Order order) {
        return deliveryPartnerList.stream()
                .filter(deliveryPartner -> deliveryPartner.getStatus() == DeliveryPartnerStatus.FREE)
                .min((dp1, dp2) -> {
                    double distance1 = getDistance(dp1.getLocation(), order.getLocation());
                    double distance2 = getDistance(dp2.getLocation(), order.getLocation());
                    return Double.compare(distance1, distance2);
                });
    }
}
