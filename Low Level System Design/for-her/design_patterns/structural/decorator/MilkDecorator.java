package design_patterns.structural.decorator;

public class MilkDecorator extends CoffeeDecorator {
    public MilkDecorator(Coffee coffee) {
        super(coffee);
    }

    @Override
    public double cost() {
        double cost = super.cost() + 0.5;
        System.out.println("Milk Added.");
        return cost;
    }
}
