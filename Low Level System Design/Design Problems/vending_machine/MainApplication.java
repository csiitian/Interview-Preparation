package system_design.vending_machine;

public class MainApplication {
    public static void main(String[] args) {

        VendingMachine vendingMachine = new VendingMachine();
        vendingMachine.getIdleState().selectItem(1);
    }
}
