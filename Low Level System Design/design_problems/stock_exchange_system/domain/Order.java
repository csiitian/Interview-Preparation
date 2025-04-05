package design_problems.stock_exchange_system.domain;

public abstract class Order {
    String id;
    User user;
    Stock stock;
    Double price;
    Long quantity;
    OrderStatus status;

    abstract boolean execute();
}
