package design_problems.vending_machine.state;

import design_problems.vending_machine.Product;
import design_problems.vending_machine.VendingMachine;

public class IdleState implements VendingMachineState {

    private final VendingMachine vendingMachine;

    public IdleState(VendingMachine vendingMachine) {
        this.vendingMachine = vendingMachine;
    }

    @Override
    public void selectProduct(Product product) {
        if (vendingMachine.getInventory().hasProduct(product)) {
            vendingMachine.setSelectedProduct(product);
            vendingMachine.setState(vendingMachine.getInsertCoinState());
            System.out.println(product + " selected");
        } else {
            System.out.println(product + " is out of stock.");
            vendingMachine.setState(vendingMachine.getIdleState());
        }
    }

    @Override
    public void insertCoin(int amount) {
        throw new UnsupportedOperationException("Invalid operation");
    }

    @Override
    public void dispenseItem() {
        throw new UnsupportedOperationException("Invalid operation");
    }

    @Override
    public void returnMoney() {
        throw new UnsupportedOperationException("Invalid operation");
    }
}
