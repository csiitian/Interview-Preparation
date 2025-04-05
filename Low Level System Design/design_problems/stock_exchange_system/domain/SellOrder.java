package design_problems.stock_exchange_system.domain;

public class SellOrder extends Order {

    @Override
    boolean execute() {
        Account account = user.getAccount();
        Portfolio portfolio = user.getPortfolio();
        if (stock.price <= price) {
            System.out.println("Sell Order executed successfully.");
            // update account balance
            account.deposit(price * quantity);
            // update portfolio
            portfolio.removeHolding(stock, quantity);
            return true;
        }
        return false;
    }
}
