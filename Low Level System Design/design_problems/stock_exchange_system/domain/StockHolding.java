package design_problems.stock_exchange_system.domain;

public class StockHolding {
    Stock stock;
    Long quantity;
    Double price;

    public StockHolding(Stock stock, Long quantity, Double price) {
        this.stock = stock;
        this.quantity = quantity;
        this.price = price;
    }

    public Stock getStock() {
        return stock;
    }

    public Long getQuantity() {
        return quantity;
    }

    public Double getPrice() {
        return price;
    }
}
