package system_design.vending_machine.state;

import system_design.vending_machine.VendingMachine;
import system_design.vending_machine.VendingMachineState;

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
    public void selectItem(int item) {
        throw new UnsupportedOperationException("Invalid operation");
    }

    @Override
    public void insertCoin(int amount) {
        throw new UnsupportedOperationException("Invalid operation");
    }

    @Override
    public void dispenseItem() {
        System.out.println("Vending Machine is in dispense item state.");
        vendingMachine.getIdleState().idle();
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
