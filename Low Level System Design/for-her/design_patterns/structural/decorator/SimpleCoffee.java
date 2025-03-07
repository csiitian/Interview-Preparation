package design_patterns.structural.decorator;

public class SimpleCoffee implements Coffee {
    @Override
    public double cost() {
        return 20.0;
    }
}
