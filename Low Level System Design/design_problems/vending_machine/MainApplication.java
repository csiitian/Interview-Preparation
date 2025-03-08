package design_problems.vending_machine;

import java.util.List;

public class MainApplication {
    public static void main(String[] args) {
        VendingMachine vendingMachine = VendingMachine.getInstance();
        vendingMachine.addInventory(List.of(Product.COKE, Product.SODA, Product.PEPSI));
        vendingMachine.selectProduct(Product.COKE);
        vendingMachine.insertCoin(30);
        vendingMachine.dispenseItem();

        System.out.println("------------------------------");

        vendingMachine.selectProduct(Product.COKE);
        vendingMachine.insertCoin(30);
        vendingMachine.dispenseItem();

        System.out.println("------------------------------");

        vendingMachine.addInventory(List.of(Product.COKE));
        vendingMachine.selectProduct(Product.COKE);
        vendingMachine.insertCoin(25);
        vendingMachine.dispenseItem();
    }
}
