package design_problems.recommendation_system.strategy;

import design_problems.recommendation_system.Product;
import design_problems.recommendation_system.User;

import java.util.List;

public class TrendingStrategy implements RecommendationStrategy {
    List<Product> trendingProducts;

    public TrendingStrategy(List<Product> trendingProducts) {
        this.trendingProducts = trendingProducts;
    }

    @Override
    public List<Product> generateRecommendations(User user) {
        return trendingProducts;
    }
}
