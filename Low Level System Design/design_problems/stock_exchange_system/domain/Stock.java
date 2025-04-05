package design_problems.stock_exchange_system.domain;

public class Stock {
    String symbol;
    String name;
    Double price;
    Long lastUpdatedAt;

    public void updatePrice(double newPrice) {
        price = newPrice;
        lastUpdatedAt = System.currentTimeMillis();
    }
}
