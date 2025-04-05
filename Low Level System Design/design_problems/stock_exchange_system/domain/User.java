package design_problems.stock_exchange_system.domain;

public class User {
    String id;
    String name;
    String email;
    String password;
    Account account;
    Portfolio portfolio;

    public Account getAccount() {
        return account;
    }

    public Portfolio getPortfolio() {
        return portfolio;
    }
}
