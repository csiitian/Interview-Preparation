package design_problems.stock_exchange_system.domain;

public class BuyOrder extends Order {

    @Override
    boolean execute() {
        Account account = user.getAccount();
        Portfolio portfolio = user.getPortfolio();
        if (account.balance < price * quantity) {
            return false;
        }
        if (stock.price <= price) {
            System.out.println("Buy Order executed successfully.");
            // update account balance
            account.withdraw(price * quantity);
            // update portfolio
            portfolio.addHolding(stock, quantity, price);
            return true;
        }
        return false;
    }
}
