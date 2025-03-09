package design_patterns.behavioral.strategy;

import java.time.LocalDateTime;
import java.util.UUID;

public class Product {
    String id;
    String name;
    double rating;
    LocalDateTime createdAt;

    public Product(String name, double rating) {
        this.id = UUID.randomUUID().toString();
        this.name = name;
        this.rating = rating;
        this.createdAt = LocalDateTime.now();
    }

    @Override
    public String toString() {
        return "Product{" +
                "id='" + id + '\'' +
                ", name='" + name + '\'' +
                ", rating=" + rating +
                '}';
    }
}
