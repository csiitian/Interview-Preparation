package design_patterns.behavioral.strategy;

import java.util.List;

public interface ISortingStrategy {
    List<Product> sort(List<Product> products);
}
