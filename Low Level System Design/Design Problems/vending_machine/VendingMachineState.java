package system_design.vending_machine;

public interface VendingMachineState {
    void idle();
    void selectItem(int item);
    void insertCoin(int amount);
    void dispenseItem();
    void returnMoney();
    void exit();
    void reset();
    void addCoin(int amount);
    void maintenance();
    void insufficientMoney();
}

/*

State
1. Idle State
2. Insert Coin State
3. Select Item State
4. Item Dispensed State
5. Money Returned State
6. Insufficient Money State
7. Maintenance State
8. Exit State
9. Reset State
10. Extra Money Inserted State

 */