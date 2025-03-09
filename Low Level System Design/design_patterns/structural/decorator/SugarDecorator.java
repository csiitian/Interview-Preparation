package design_patterns.structural.decorator;

public class SugarDecorator extends CoffeeDecorator {
    public SugarDecorator(Coffee coffee) {
        super(coffee);
    }

    @Override
    public double cost() {
        double cost = super.cost() + 0.2;
        System.out.println("Sugar Added.");
        return cost;
    }
}
