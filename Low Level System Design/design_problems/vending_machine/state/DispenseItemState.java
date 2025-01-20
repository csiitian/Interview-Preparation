package design_problems.vending_machine.state;

import design_problems.vending_machine.Product;
import design_problems.vending_machine.VendingMachine;
import design_problems.vending_machine.VendingMachineState;

public class DispenseItemState implements VendingMachineState {

    private final VendingMachine vendingMachine;

    public DispenseItemState(VendingMachine vendingMachine) {
        this.vendingMachine = vendingMachine;
    }

    @Override
    public void idle() {
        throw new UnsupportedOperationException("Invalid operation");
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

    @Override
    public void exit() {
        throw new UnsupportedOperationException("Invalid operation");
    }

    @Override
    public void reset() {
        throw new UnsupportedOperationException("Invalid operation");
    }

    @Override
    public void addCoin(int amount) {
        throw new UnsupportedOperationException("Invalid operation");
    }

    @Override
    public void maintenance() {
        throw new UnsupportedOperationException("Invalid operation");
    }

    @Override
    public void insufficientMoney() {
        throw new UnsupportedOperationException("Invalid operation");
    }
}
