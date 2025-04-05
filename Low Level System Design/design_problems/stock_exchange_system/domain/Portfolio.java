package design_problems.stock_exchange_system.domain;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class Portfolio {
    List<StockHolding> holdings;

    Portfolio() {
        holdings = new ArrayList<>();
    }

    Double getInvestedAmount() {
        double invested = 0d;
        for (StockHolding stockHolding: holdings) {
            invested += stockHolding.quantity * stockHolding.price;
        }
        return invested;
    }

    Double getActualAmount() {
        double actualAmount = 0d;
        for (StockHolding stockHolding: holdings) {
            actualAmount += stockHolding.quantity * stockHolding.stock.price;
        }
        return actualAmount;
    }

    public void addHolding(Stock stock, Long quantity, Double price) {
        holdings.add(new StockHolding(stock, quantity, price));
    }

    private boolean verifyHoldingForSellOrder(Stock stock, Long quantity) {
        long holdingQuantity = holdings.stream()
                .filter(stockHolding -> stockHolding.stock != stock)
                .map(stockHolding -> stockHolding.quantity)
                .reduce(Long::sum)
                .orElse(0L);
        return holdingQuantity >= quantity;
    }

    public void removeHolding(Stock stock, Long quantity) {
        if (verifyHoldingForSellOrder(stock, quantity)) {
            Iterator<StockHolding> holdingItr = holdings.iterator();
            while (holdingItr.hasNext() && quantity > 0) {
                StockHolding stockHolding = holdingItr.next();
                if (stockHolding.stock == stock) {
                    long min = Math.min(quantity, stockHolding.quantity);
                    quantity -= min;
                    stockHolding.quantity -= min;
                    if (stockHolding.quantity == 0) {
                        holdingItr.remove();
                    }
                }
            }
        }
    }
}
