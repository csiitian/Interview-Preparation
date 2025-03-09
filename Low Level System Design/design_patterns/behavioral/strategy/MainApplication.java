package design_patterns.behavioral.strategy;

import java.util.List;

public class MainApplication {
    public static void main(String[] args) {
        List<Product> products = List.of(
                new Product("Apple", 3.5),
                new Product("Grapes", 3.2),
                new Product("StrawBerry", 4.5),
                new Product("Coconut", 5.0),
                new Product("Banana", 3.5),
                new Product("Orange", 2.5)
        );

        ProductService productService = new ProductService(new AscSortingStrategy(), products);
        List<Product> sortedProducts = productService.viewProducts();
        System.out.println(sortedProducts);

        productService.setSortingStrategy(new DescSortingStrategy());
        sortedProducts = productService.viewProducts();
        System.out.println(sortedProducts);
    }
}
