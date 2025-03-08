package design_problems.recommendation_system.strategy;

import design_problems.recommendation_system.Interaction;
import design_problems.recommendation_system.Product;
import design_problems.recommendation_system.User;

import java.util.*;
import java.util.stream.Collectors;

public class CollaborativeFilteringStrategy implements RecommendationStrategy {
    final List<Product> allProducts;
    final Map<String, List<User>> userGroups; // tag -> list of user mapping

    public CollaborativeFilteringStrategy(List<Product> allProducts, Map<String, List<User>> userGroups) {
        this.allProducts = allProducts;
        this.userGroups = userGroups;
    }

    @Override
    public List<Product> generateRecommendations(User user) {
        Set<String> productIds = user.getPreferencesList().stream()
                .map(preference -> userGroups.getOrDefault(preference, Collections.emptyList()))
                .flatMap(List::stream)
                .flatMap(similarUser -> similarUser.getInteractions().stream())
                .map(Interaction::getProductId)
                .collect(Collectors.toSet());

        return allProducts.stream()
                .filter(product -> productIds.contains(product.getId()))
                .toList();
    }
}
