package design_problems.vending_machine.state;

import design_problems.vending_machine.Product;
import design_problems.vending_machine.VendingMachine;

public class DispenseItemState implements VendingMachineState {

    private final VendingMachine vendingMachine;

    public DispenseItemState(VendingMachine vendingMachine) {
        this.vendingMachine = vendingMachine;
    }

    @Override
    public void selectProduct(Product product) {
        throw new UnsupportedOperationException("Invalid operation");
    }

    @Override
    public void insertCoin(int amount) {
        throw new UnsupportedOperationException("Invalid operation");
    }

    @Override
    public void dispenseItem() {
        System.out.println(vendingMachine.getSelectedProduct() + " is dispensing");
        vendingMachine.getInventory().removeProduct(vendingMachine.getSelectedProduct(), 1);
        vendingMachine.setState(vendingMachine.getReturnChangeState());
    }

    @Override
    public void returnMoney() {
        throw new UnsupportedOperationException("Invalid operation");
    }
}
