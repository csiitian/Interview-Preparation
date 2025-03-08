package design_problems.recommendation_system.strategy;

import design_problems.recommendation_system.Product;
import design_problems.recommendation_system.User;

import java.util.List;

public class ContentBasedStrategy implements RecommendationStrategy {
    List<Product> allProducts;

    public ContentBasedStrategy(List<Product> allProducts) {
        this.allProducts = allProducts;
    }

    @Override
    public List<Product> generateRecommendations(User user) {
        return allProducts.stream().filter(product -> product.getTags().stream().anyMatch(tag -> user.getPreferencesList().contains(tag))).toList();
    }
}
