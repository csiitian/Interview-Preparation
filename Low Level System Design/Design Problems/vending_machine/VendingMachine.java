package system_design.vending_machine;

import system_design.vending_machine.state.DispenseItemState;
import system_design.vending_machine.state.IdleState;
import system_design.vending_machine.state.InsertCoinState;
import system_design.vending_machine.state.SelectItemState;

public class VendingMachine {
    private final IdleState idleState;
    private final SelectItemState selectItemState;
    private final InsertCoinState insertCoinState;
    private final DispenseItemState dispenseItemState;
    private volatile VendingMachineState vendingMachineState;

    public VendingMachine() {
        idleState = new IdleState(this);
        selectItemState = new SelectItemState(this);
        insertCoinState = new InsertCoinState(this);
        dispenseItemState = new DispenseItemState(this);
        vendingMachineState = idleState;
    }

    public VendingMachineState getIdleState() {
        return idleState;
    }

    public VendingMachineState getSelectItemState() {
        return selectItemState;
    }

    public VendingMachineState getInsertCoinState() {
        return insertCoinState;
    }

    public VendingMachineState getDispenseItemState() {
        return dispenseItemState;
    }
}
