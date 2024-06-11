# Best Time to Buy and Sell Stock

```java
class Solution {
    public int maxProfit(int[] prices) {
        int min = Integer.MAX_VALUE;
        int profit = 0;
        for(int price: prices) {
            profit = Math.max(profit, price-min);
            min = Math.min(min, price);
        }
        return profit;
    }
}
```

# Best Time to Buy and Sell Stock II

```java
class Solution {
    public int maxProfit(int[] prices) {
        int n = prices.length;
        int profit = 0;
        int prev = prices[0];
        for(int price: prices) {
            if(price > prev) 
                profit += price - prev;
            prev = price;
        }
        return profit;
    }
}
```

# Best Time to Buy and Sell Stock III
 
```java
class Solution {
    public int maxProfit(int[] prices) {
        int n = prices.length;
        int profit[] = new int[n];
        int min = Integer.MAX_VALUE;
        for(int i=0;i<n;i++) {
            if(min > prices[i]) min = prices[i];
            profit[i] = prices[i] - min;
        }

        int max = Integer.MIN_VALUE;
        int maxProfit = 0;
        for(int i=n-1;i>=0;i--) {
            if(max < prices[i]) max = prices[i];
            maxProfit = Math.max(maxProfit, max - prices[i]);
            profit[i] += maxProfit;
        }

        int ans = 0;
        for(int x: profit) {
            if(ans < x) ans = x;
        }
        return ans;
    }
}
```

# Best Time to Buy and Sell Stock IV

### Top Down Approach

```java
class Solution {
    Integer[][][] memo;
    public int maxProfit(int k, int[] prices) {
        int n = prices.length;        
        memo = new Integer[k+1][n+1][2];
        return solve(k, 0, 0, prices);
    }

    public int solve(int k, int ind, int buy, int[] prices) {
        if(ind >= prices.length) return 0;
        if(k == 0) return 0;
        if(memo[k][ind][buy] != null)
            return memo[k][ind][buy];
        if(buy == 1) {
            // sell it if possible
            return memo[k][ind][buy] = Math.max(
                prices[ind] + solve(k - 1, ind+1, 0, prices),
                solve(k, ind+1, 1, prices)
            );
        } else {
            // buy it or don't buy it
            return memo[k][ind][buy] = Math.max(
                -prices[ind] + solve(k, ind+1, 1, prices),
                solve(k, ind+1, 0, prices)
            );
        }
    }
}
```

# Maximum Profit From Trading Stocks

```java
class Solution {
    Integer[][] memo;
    public int maximumProfit(int[] present, int[] future, int budget) {
        int n = present.length;
        memo = new Integer[budget+1][n];
        return helper(present, future, budget, 0);
    }

    public int helper(int[] present, int[] future, int budget, int curr) {
        if(curr == present.length || budget < 0) return 0;
        if(memo[budget][curr] != null) {
            return memo[budget][curr];
        }
        int ans = 0;
        if(present[curr] <= budget && future[curr] - present[curr] > 0) {
            ans = Math.max(
                ans, 
                future[curr] - present[curr] + helper(present, future, budget - present[curr], curr+1)
            );
        }
        ans = Math.max(ans, helper(present, future, budget, curr+1));
        return memo[budget][curr] = ans;
    }
}
```

# Best Time to Buy and Sell Stock with Transaction Fee

```java
class Solution {
    Integer[][] memo;
    public int maxProfit(int[] prices, int fee) {
        int n = prices.length;
        memo = new Integer[n][2];
        return helper(prices, fee, 0, 0);
    }

    public int helper(int[] prices, int fee, int curr, int buy) {
        if(curr == prices.length) return 0;
        if(memo[curr][buy] != null) return memo[curr][buy];
        if(buy == 1) {
            // sell it with transaction fee
            return memo[curr][buy] = Math.max(
                -fee + prices[curr] + helper(prices, fee, curr+1, 0),
                helper(prices, fee, curr+1, 1)
            );
        } else {
            return memo[curr][buy] = Math.max(
                -prices[curr] + helper(prices, fee, curr+1, 1),
                helper(prices, fee, curr+1, 0)
            );
        }  
    }
}
```

# Best Time to Buy and Sell Stock with Cooldown

```java
class Solution {
    Integer[][] memo;
    public int maxProfit(int[] prices) {
        int n = prices.length;
        memo = new Integer[n][2];
        return helper(prices, 0, 0);
    }

    public int helper(int[] prices, int curr, int buy) {
        if(curr >= prices.length) return 0;
        if(memo[curr][buy] != null) return memo[curr][buy];
        if(buy == 1) {
            return memo[curr][buy] = Math.max(
                prices[curr] + helper(prices, curr+2, 0),
                helper(prices, curr+1, 1)
            );
        } else {
            return memo[curr][buy] = Math.max(
                -prices[curr] + helper(prices, curr+1, 1),
                helper(prices, curr+1, 0)
            );
        }
    }
}
```
