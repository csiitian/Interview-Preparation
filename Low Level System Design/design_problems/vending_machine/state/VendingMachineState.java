package design_problems.vending_machine.state;

import design_problems.vending_machine.Product;

public interface VendingMachineState {
    void selectProduct(Product product);
    void insertCoin(int amount);
    void dispenseItem();
    void returnMoney();
}