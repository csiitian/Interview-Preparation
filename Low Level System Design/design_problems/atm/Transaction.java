public class Transaction {
    private String transactionType;
    private double amount;
    private Account account;

    public Transaction(String transactionType, double amount, Account account) {
        this.transactionType = transactionType;
        this.amount = amount;
        this.account = account;
    }

    public boolean execute() {
        switch (transactionType) {
            case "deposit":
                return account.deposit(amount);
            case "withdraw":
                return account.withdraw(amount);
            default:
                return false;
        }
    }
}