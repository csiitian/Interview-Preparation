package design_problems.recommendation_system.strategy;

import design_problems.recommendation_system.Product;
import design_problems.recommendation_system.User;

import java.util.List;

public interface RecommendationStrategy {
    List<Product> generateRecommendations(User user);
}
