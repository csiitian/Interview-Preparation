package design_patterns.behavioral.strategy;

import java.util.List;

public class DescSortingStrategy implements ISortingStrategy {

    @Override
    public List<Product> sort(List<Product> products) {
        return products.stream()
                .sorted((p1, p2) -> Double.compare(p2.rating, p1.rating))
                .toList();
    }
}
