package design_problems.stock_exchange_system.domain;

import design_problems.stock_exchange_system.exception.InsufficientBalanceException;

public class Account {
    String id;
    String accountNo;
    Double balance;

    public void deposit(double amount) {
        balance += amount;
    }

    public void withdraw(double amount) {
        if (amount > balance) {
            throw new InsufficientBalanceException("user total balance is " + balance);
        }
        synchronized (this) {
            balance -= amount;
        }
    }
}
