package design_problems.vending_machine;

public enum Product {
    COKE("Coke", 25.0),
    PEPSI("Pepsi", 35.0),
    SODA("Soda", 45.0);

    private final String name;
    private final double price;

    Product(String name, double price) {
        this.name = name;
        this.price = price;
    }

    public String getName() {
        return name;
    }

    public double getPrice() {
        return price;
    }

    public static Product getProduct(int item) {
        return switch (item) {
            case 1 -> COKE;
            case 2 -> PEPSI;
            case 3 -> SODA;
            default -> null;
        };
    }

    @Override
    public String toString() {
        return name + " - $" + price;
    }
}
