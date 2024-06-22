package system_design.vending_machine.state;

import system_design.vending_machine.VendingMachine;
import system_design.vending_machine.VendingMachineState;

public class InsertCoinState implements VendingMachineState {
    
    private final VendingMachine vendingMachine;

    public InsertCoinState(VendingMachine vendingMachine) {
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
        System.out.println("Vending Machine is in insert coin state.");
        vendingMachine.getDispenseItemState().dispenseItem();
    }

    @Override
    public void dispenseItem() {
        throw new UnsupportedOperationException("Invalid operation");
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
