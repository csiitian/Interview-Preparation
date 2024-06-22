package system_design.vending_machine.state;

import system_design.vending_machine.VendingMachine;
import system_design.vending_machine.VendingMachineState;

import java.util.Scanner;

public class IdleState implements VendingMachineState {

    private final VendingMachine vendingMachine;

    public IdleState(VendingMachine vendingMachine) {
        this.vendingMachine = vendingMachine;
    }

    @Override
    public void idle() {
        System.out.println("Vending Machine is in idle state.");
        Scanner sc = new Scanner(System.in);
    }

    @Override
    public void selectItem(int item) {
        vendingMachine.getSelectItemState().selectItem(item);
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
