package design_patterns.behavioral.strategy;

import java.util.Comparator;
import java.util.List;

public class AscSortingStrategy implements ISortingStrategy {

    @Override
    public List<Product> sort(List<Product> products) {
        return products.stream()
                .sorted(Comparator.comparing(p -> p.rating))
                .toList();
    }
}
