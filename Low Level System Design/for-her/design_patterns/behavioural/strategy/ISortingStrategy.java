package design_patterns.behavioural.strategy;

import java.util.List;

public interface ISortingStrategy {
    List<Product> sort(List<Product> products);
}
