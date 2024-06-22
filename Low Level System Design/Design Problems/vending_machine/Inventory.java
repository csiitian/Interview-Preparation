package system_design.vending_machine;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class Inventory {
    private final Map<Product, Integer> products;

    public Inventory() {
        this.products = new ConcurrentHashMap<>();
    }

    public void addProduct(Product product, int quantity) {
        products.put(product, products.getOrDefault(product, 0) + quantity);
    }

    public void removeProduct(Product product, int quantity) {
        if(products.containsKey(product) && products.get(product) >= quantity) {
            products.put(product, products.get(product) - quantity);
        }
        throw new IllegalArgumentException("Product not found or quantity is not enough");
    }

    public int getQuantity(Product product) {
        return products.getOrDefault(product, 0);
    }

    public boolean hasProduct(Product product) {
        return products.containsKey(product) && products.get(product) > 0;
    }
}
