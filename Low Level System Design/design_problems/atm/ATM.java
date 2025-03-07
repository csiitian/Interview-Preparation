import java.util.HashMap;
import java.util.Map;

public class ATM {
    private Map<String, Account> accounts;

    public ATM() {
        accounts = new HashMap<>();
    }

    public void addAccount(Account account) {
        accounts.put(account.getAccountNumber(), account);
    }

    public Account authenticate(String accountNumber, String pin) {
        Account account = accounts.get(accountNumber);
        if (account != null && account.validatePin(pin)) {
            return account;
        }
        return null;
    }

    public boolean performTransaction(Account account, String transactionType, double amount) {
        Transaction transaction = new Transaction(transactionType, amount, account);
        return transaction.execute();
    }
}
