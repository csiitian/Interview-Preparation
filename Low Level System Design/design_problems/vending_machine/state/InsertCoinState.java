package design_problems.vending_machine.state;

import design_problems.vending_machine.Product;
import design_problems.vending_machine.VendingMachine;

public class InsertCoinState implements VendingMachineState {

    private final VendingMachine vendingMachine;

    public InsertCoinState(VendingMachine vendingMachine) {
        this.vendingMachine = vendingMachine;
    }

    @Override
    public void selectProduct(Product product) {
        throw new UnsupportedOperationException("Invalid operation");
    }

    @Override
    public void insertCoin(int amount) {
        System.out.println(amount + " coins inserted.");
        vendingMachine.addCollectedCoins(amount);
        if (checkPaymentStatus()) {
            vendingMachine.setState(vendingMachine.getDispenseItemState());
        }
    }

    private boolean checkPaymentStatus() {
        return vendingMachine.getCollectedCoins() >= vendingMachine.getSelectedProduct().getPrice();
    }

    @Override
    public void dispenseItem() {
        System.out.println("Please add more coins.");
    }

    @Override
    public void returnMoney() {
        throw new UnsupportedOperationException("Invalid operation");
    }
}
