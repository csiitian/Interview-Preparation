package design_patterns.structural.decorator;

public class MainApplication {
    public static void main(String[] args) {
        Coffee coffee = new SimpleCoffee();
        CoffeeDecorator coffeeDecorator = new MilkDecorator(new SugarDecorator(new SugarDecorator(coffee)));
        double totalCost = coffeeDecorator.cost();
        System.out.println("Total Cost: " + totalCost);
    }
}
