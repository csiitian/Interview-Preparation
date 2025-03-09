package design_patterns.structural.decorator;

public abstract class CoffeeDecorator implements Coffee {
    Coffee coffee;

    public CoffeeDecorator(Coffee coffee) {
        this.coffee = coffee;
    }

    @Override
    public double cost() {
        return coffee.cost();
    }
}
