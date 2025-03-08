package design_problems.vending_machine;

import design_problems.vending_machine.state.*;

import java.util.List;

public class VendingMachine {
    private volatile static VendingMachine instance;
    private final IdleState idleState;
    private final InsertCoinState insertCoinState;
    private final DispenseItemState dispenseItemState;
    private final ReturnChangeState returnChangeState;
    private volatile VendingMachineState vendingMachineState;
    Inventory inventory;
    int coinInventory;
    Product selectedProduct;
    int collectedCoins;

    private VendingMachine() {
        idleState = new IdleState(this);
        insertCoinState = new InsertCoinState(this);
        dispenseItemState = new DispenseItemState(this);
        returnChangeState = new ReturnChangeState(this);
        vendingMachineState = idleState;
        inventory = new Inventory();
        coinInventory = 0;
        collectedCoins = 0;
    }

    public static synchronized VendingMachine getInstance() {
        if (instance == null) {
            instance = new VendingMachine();
        }
        return instance;
    }

    public void setState(VendingMachineState state) {
        this.vendingMachineState = state;
    }

    public IdleState getIdleState() {
        return idleState;
    }

    public InsertCoinState getInsertCoinState() {
        return insertCoinState;
    }

    public DispenseItemState getDispenseItemState() {
        return dispenseItemState;
    }

    public ReturnChangeState getReturnChangeState() {
        return returnChangeState;
    }

    public int getCollectedCoins() {
        return collectedCoins;
    }

    public Product getSelectedProduct() {
        return selectedProduct;
    }

    public void setSelectedProduct(Product selectedProduct) {
        this.selectedProduct = selectedProduct;
    }

    public void setCollectedCoins(int collectedCoins) {
        this.collectedCoins = collectedCoins;
    }

    public void addCollectedCoins(int collectedCoins) {
        this.collectedCoins += collectedCoins;
    }

    public void addInventory(List<Product> products) {
        for (Product product : products) {
            inventory.addProduct(product, 1);
        }
    }

    public Inventory getInventory() {
        return inventory;
    }

    public void addCoinInventory(int amount) {
        coinInventory += amount;
    }

    public void selectProduct(Product product) {
        try {
            vendingMachineState.selectProduct(product);
        } catch (Exception e) {
            System.out.println("Error while selecting product: " + e.getMessage());
        }
    }

    public void insertCoin(int amount) {
        try {
            vendingMachineState.insertCoin(amount);
        } catch (Exception e) {
            System.out.println("Error while inserting coin: " + e.getMessage());
        }
    }

    public void dispenseItem() {
        try {
            vendingMachineState.dispenseItem();
        } catch (Exception e) {
            System.out.println("Error while dispensing item: " + e.getMessage());
        }
        try {
            vendingMachineState.returnMoney();
        } catch (Exception e) {
            System.out.println("Error while returning money: " + e.getMessage());
        }
    }
}
