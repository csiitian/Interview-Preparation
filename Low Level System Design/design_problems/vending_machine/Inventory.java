package design_problems.vending_machine;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class Inventory {
    private final Map<String, Integer> products;

    public Inventory() {
        this.products = new ConcurrentHashMap<>();
    }

    public void addProduct(Product product, int quantity) {
        products.put(product.name(), products.getOrDefault(product.name(), 0) + quantity);
    }

    public void removeProduct(Product product, int quantity) {
        if (products.containsKey(product.name()) && products.get(product.name()) >= quantity) {
            products.put(product.name(), products.get(product.name()) - quantity);
        } else {
            throw new IllegalArgumentException("Product not found or quantity is not enough");
        }
    }

    public int getQuantity(Product product) {
        return products.getOrDefault(product.name(), 0);
    }

    public boolean hasProduct(Product product) {
        return products.containsKey(product.name()) && products.get(product.name()) > 0;
    }
}
