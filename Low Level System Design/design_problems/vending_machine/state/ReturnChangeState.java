package design_problems.vending_machine.state;

import design_problems.vending_machine.Product;
import design_problems.vending_machine.VendingMachine;

public class ReturnChangeState implements VendingMachineState {

    private final VendingMachine vendingMachine;

    public ReturnChangeState(VendingMachine vendingMachine) {
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
        throw new UnsupportedOperationException("Invalid operation");
    }

    @Override
    public void returnMoney() {
      double remainingCoins = vendingMachine.getCollectedCoins() - vendingMachine.getSelectedProduct().getPrice();
      if (remainingCoins > 0) {
          System.out.println("returned " + remainingCoins);
          vendingMachine.setCollectedCoins(0);
      }
      vendingMachine.setState(vendingMachine.getIdleState());
    }
}
