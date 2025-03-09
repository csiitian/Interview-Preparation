package design_patterns.behavioral.strategy;

import java.util.ArrayList;
import java.util.List;

public class ProductService {
    ISortingStrategy sortingStrategy;
    List<Product> products;

    public ProductService(ISortingStrategy sortingStrategy) {
        this.sortingStrategy = sortingStrategy;
        this.products = new ArrayList<>();
    }

    public ProductService(ISortingStrategy sortingStrategy, List<Product> products) {
        this.sortingStrategy = sortingStrategy;
        this.products = products;
    }

    void setSortingStrategy(ISortingStrategy sortingStrategy) {
        this.sortingStrategy = sortingStrategy;
    }

    void addProduct(Product product) {
        products.add(product);
    }

    List<Product> viewProducts() {
        return sortingStrategy.sort(products);
    }
}
