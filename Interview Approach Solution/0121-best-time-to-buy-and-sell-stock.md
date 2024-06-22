### Interviewer and Interviewee Discussion

**Interviewer:** Let's go through a common stock trading problem. You're given an array `prices` where `prices[i]` represents the price of a stock on the `i-th` day. Your goal is to maximize your profit by choosing a single day to buy one stock and a different day in the future to sell that stock. You need to return the maximum profit you can achieve from this transaction. If it is impossible to achieve any profit, return `0`. How would you approach solving this?

**Interviewee:** I'll start by clarifying the requirements. We need to find the maximum difference between any subsequent prices in the array such that the later price is higher than the earlier one. This difference represents the profit. If none of the subsequent prices are higher, the profit would be zero.

**Interviewer:** That's correct. Let's think about the brute force approach first. How would you implement it and what will be its complexity?

**Interviewee:** Well, the brute force method would involve checking every possible pair of buy and sell days. We would:
- Loop through each day as the buying day.
- For each buying day, loop through each subsequent day as the selling day.
- Calculate the profit for each pair and track the maximum profit.

Here’s how we can write it in pseudocode:

```python
max_profit = 0
for i in range(len(prices)):
    for j in range(i+1, len(prices)):
        profit = prices[j] - prices[i]
        max_profit = max(max_profit, profit)
return max_profit
```

**Interviewer:** That makes sense. What about its time and space complexity?

**Interviewee:** The time complexity of this approach would be `O(n^2)` because we are using two nested loops over the list of prices where `n` is the length of the prices array. As for space complexity, it’s `O(1)` because we are using a constant amount of additional space.

### Optimizing the Solution

**Interviewer:** The brute force approach works, but can you think of a way to optimize it?

**Interviewee:** Sure. We can improve the efficiency by keeping track of the minimum price encountered so far while iterating through the list. This way, we can always consider the maximum profit that can be attained by selling on the current day without the need for nested loops.

Here's how the optimized approach looks:

1. Initialize two variables: `min_price` to infinity and `max_profit` to 0.
2. Iterate through each price in the list.
   - Update `min_price` to be the minimum of `min_price` and the current price.
   - Calculate `current_profit` as the difference between the current price and `min_price`.
   - Update `max_profit` to be the maximum of `max_profit` and `current_profit`.

Let’s see the pseudocode:

```python
min_price = float('inf')
max_profit = 0
for price in prices:
    min_price = min(min_price, price)
    current_profit = price - min_price
    max_profit = max(max_profit, current_profit)
return max_profit
```

**Interviewer:** That looks good. Can you explain the time and space complexity for this approach?

**Interviewee:** The time complexity of this solution is `O(n)` because we are iterating through the prices array exactly once. The space complexity is `O(1)` since we are using a constant amount of extra space for our variables `min_price` and `max_profit`.

**Interviewer:** Excellent. Can you also illustrate this with a small example to see how it works step-by-step?

**Interviewee:** Sure, let’s use the example `prices = [7, 1, 5, 3, 6, 4]`.

- Initialize `min_price` to infinity and `max_profit` to 0.
- Start iterating:
  - Day 1: Price = 7
    - `min_price` = 7
    - `current_profit` = 0 (7 - 7)
    - `max_profit` = 0
  - Day 2: Price = 1
    - `min_price` = 1
    - `current_profit` = 0 (1 - 1)
    - `max_profit` = 0
  - Day 3: Price = 5
    - `min_price` = 1
    - `current_profit` = 4 (5 - 1)
    - `max_profit` = 4
  - Day 4: Price = 3
    - `min_price` = 1
    - `current_profit` = 2 (3 - 1)
    - `max_profit` = 4
  - Day 5: Price = 6
    - `min_price` = 1
    - `current_profit` = 5 (6 - 1)
    - `max_profit` = 5
  - Day 6: Price = 4
    - `min_price` = 1
    - `current_profit` = 3 (4 - 1)
    - `max_profit` = 5

So, the maximum profit here is 5, which matches our expected result.
Certainly! Let me translate the optimized algorithm to the provided code snippets and include both the time and space complexity comments.

### C++

```cpp
class Solution {
public:
    int maxProfit(vector<int>& prices) {
        int min_price = INT_MAX;
        int max_profit = 0;
        for (int price : prices) {
            if (price < min_price) {
                min_price = price;
            } else if (price - min_price > max_profit) {
                max_profit = price - min_price;
            }
        }
        return max_profit;
    }
};

// Time complexity: O(n)
// Space complexity: O(1)
```

### Java

```java
class Solution {
    public int maxProfit(int[] prices) {
        int min_price = Integer.MAX_VALUE;
        int max_profit = 0;
        for (int price : prices) {
            if (price < min_price) {
                min_price = price;
            } else if (price - min_price > max_profit) {
                max_profit = price - min_price;
            }
        }
        return max_profit;
    }
}

// Time complexity: O(n)
// Space complexity: O(1)
```

### Python

```python
class Solution(object):
    def maxProfit(self, prices):
        """
        :type prices: List[int]
        :rtype: int
        """
        min_price = float('inf')
        max_profit = 0
        for price in prices:
            if price < min_price:
                min_price = price
            elif price - min_price > max_profit:
                max_profit = price - min_price
        return max_profit

# Time complexity: O(n)
# Space complexity: O(1)
```

### Python3

```python
class Solution:
    def maxProfit(self, prices: List[int]) -> int:
        min_price = float('inf')
        max_profit = 0
        for price in prices:
            if price < min_price:
                min_price = price
            elif price - min_price > max_profit:
                max_profit = price - min_price
        return max_profit

# Time complexity: O(n)
# Space complexity: O(1)
```

### C

```c
int maxProfit(int* prices, int pricesSize) {
    int min_price = INT_MAX;
    int max_profit = 0;
    for (int i = 0; i < pricesSize; i++) {
        if (prices[i] < min_price) {
            min_price = prices[i];
        } else if (prices[i] - min_price > max_profit) {
            max_profit = prices[i] - min_price;
        }
    }
    return max_profit;
}

// Time complexity: O(n)
// Space complexity: O(1)
```

### C#

```csharp
public class Solution {
    public int MaxProfit(int[] prices) {
        int min_price = int.MaxValue;
        int max_profit = 0;
        foreach (int price in prices) {
            if (price < min_price) {
                min_price = price;
            } else if (price - min_price > max_profit) {
                max_profit = price - min_price;
            }
        }
        return max_profit;
    }
}

// Time complexity: O(n)
// Space complexity: O(1)
```

### JavaScript

```javascript
/**
 * @param {number[]} prices
 * @return {number}
 */
var maxProfit = function(prices) {
    let min_price = Infinity;
    let max_profit = 0;
    for (let price of prices) {
        if (price < min_price) {
            min_price = price;
        } else if (price - min_price > max_profit) {
            max_profit = price - min_price;
        }
    }
    return max_profit;
};

// Time complexity: O(n)
// Space complexity: O(1)
```

### TypeScript

```typescript
function maxProfit(prices: number[]): number {
    let min_price = Infinity;
    let max_profit = 0;
    for (let price of prices) {
        if (price < min_price) {
            min_price = price;
        } else if (price - min_price > max_profit) {
            max_profit = price - min_price;
        }
    }
    return max_profit;
}

// Time complexity: O(n)
// Space complexity: O(1)
```

### PHP

```php
class Solution {

    /**
     * @param Integer[] $prices
     * @return Integer
     */
    function maxProfit($prices) {
        $min_price = PHP_INT_MAX;
        $max_profit = 0;
        foreach ($prices as $price) {
            if ($price < $min_price) {
                $min_price = $price;
            } else if ($price - $min_price > $max_profit) {
                $max_profit = $price - $min_price;
            }
        }
        return $max_profit;
    }
}

// Time complexity: O(n)
// Space complexity: O(1)
```

### Swift

```swift
class Solution {
    func maxProfit(_ prices: [Int]) -> Int {
        var min_price = Int.max
        var max_profit = 0
        for price in prices {
            if price < min_price {
                min_price = price
            } else if price - min_price > max_profit {
                max_profit = price - min_price
            }
        }
        return max_profit
    }
}

// Time complexity: O(n)
// Space complexity: O(1)
```

### Kotlin

```kotlin
class Solution {
    fun maxProfit(prices: IntArray): Int {
        var min_price = Int.MAX_VALUE
        var max_profit = 0
        for (price in prices) {
            if (price < min_price) {
                min_price = price
            } else if (price - min_price > max_profit) {
                max_profit = price - min_price
            }
        }
        return max_profit
    }
}

// Time complexity: O(n)
// Space complexity: O(1)
```

### Dart

```dart
class Solution {
  int maxProfit(List<int> prices) {
    int min_price = double.infinity.toInt();
    int max_profit = 0;
    for (int price in prices) {
      if (price < min_price) {
        min_price = price;
      } else if (price - min_price > max_profit) {
        max_profit = price - min_price;
      }
    }
    return max_profit;
  }
}

// Time complexity: O(n)
// Space complexity: O(1)
```

### Go

```go
func maxProfit(prices []int) int {
    min_price := int(^uint(0) >> 1) // effectively sets min_price to Int Max
    max_profit := 0
    for _, price := range prices {
        if price < min_price {
            min_price = price
        } else if profit := price - min_price; profit > max_profit {
            max_profit = profit
        }
    }
    return max_profit
}

// Time complexity: O(n)
// Space complexity: O(1)
```

### Ruby

```ruby
# @param {Integer[]} prices
# @return {Integer}
def max_profit(prices)
    min_price = Float::INFINITY
    max_profit = 0
    prices.each do |price|
        if price < min_price
            min_price = price
        elsif price - min_price > max_profit
            max_profit = price - min_price
        end
    end
    max_profit
end

# Time complexity: O(n)
// Space complexity: O(1)
```

### Scala

```scala
object Solution {
    def maxProfit(prices: Array[Int]): Int = {
        var min_price = Int.MaxValue
        var max_profit = 0
        for (price <- prices) {
            if (price < min_price) {
                min_price = price
            } else if (price - min_price > max_profit) {
                max_profit = price - min_price
            }
        }
        max_profit
    }
}

// Time complexity: O(n)
// Space complexity: O(1)
```

### Rust

```rust
impl Solution {
    pub fn max_profit(prices: Vec<i32>) -> i32 {
        let mut min_price = i32::MAX;
        let mut max_profit = 0;
        for &price in prices.iter() {
            if price < min_price {
                min_price = price;
            } else if price - min_price > max_profit {
                max_profit = price - min_price;
            }
        }
        max_profit
    }
}

// Time complexity: O(n)
// Space complexity: O(1)
```

### Racket

```racket
(define/contract (max-profit prices)
  (-> (listof exact-integer?) exact-integer?)
  (define min-price +inf.0)
  (define max-profit 0)
  (for ([price (in-list prices)])
    (set! min-price (min min-price price))
    (set! max-profit (max max-profit (- price min-price))))
  max-profit
)

// Time complexity: O(n)
// Space complexity: O(1)
```

### Erlang

```erlang
-spec max_profit(Prices :: [integer()]) -> integer().
max_profit(Prices) ->
    MinPrice = lists:foldl(fun(P, Acc) -> min(P, Acc) end, 10000, Prices),
    lists:foldl(
        fun(P, {MinPrice, MaxProfit}) -> 
            {min(P, MinPrice), max(P - MinPrice, MaxProfit)} 
        end, 
        {MinPrice, 0}, Prices) 
    % Return only max profit
    |> (fun({_MinPrice, MaxProfit}) -> MaxProfit end).

% Time complexity: O(n)
% Space complexity: O(1)
```

### Elixir

```elixir
defmodule Solution do
  @spec max_profit(prices :: [integer]) :: integer
  def max_profit(prices) do
    Enum.reduce(prices, {1.0e+10 |> round, 0}, fn price, {min_price, max_profit} ->
      min_price = min(price, min_price)
      max_profit = max(price - min_price, max_profit)
      {min_price, max_profit}
    end)
    |> elem(1) # return only max_profit
  end
end

# Time complexity: O(n)
// Space complexity: O(1)
```

Each of these code snippets implements the optimized algorithm to find the maximum profit from a single buy-sell transaction given a list of stock prices. The time complexity for each is O(n), and the space complexity is O(1).


### Closing Statement

We've thoroughly discussed an essential problem in stock trading: maximizing profit by buying on one day and selling on another. We explored the brute force approach, which had a time complexity of \(O(n^2)\), and then optimized it to achieve \(O(n)\) time complexity with \(O(1)\) space complexity. This approach effectively tracks the minimum price encountered so far and calculates the potential profit at each step, ensuring we only traverse the list once.

We also translated this optimized approach into various programming languages, giving you a comprehensive toolkit to handle similar problems in different technical environments. Understanding and implementing this kind of optimization is critical for writing efficient and scalable code, especially in time-sensitive applications like financial software.

### Similar Questions to Practice

1. **Best Time to Buy and Sell Stock II**: Unlike the current problem which allows only one transaction, this problem allows multiple transactions (i.e., as many as you like). The catch is that you must sell the stock before you buy again.

2. **Best Time to Buy and Sell Stock with Cooldown**: You are not allowed to buy on the next day after selling (i.e., there is a cooldown of one day).

3. **Best Time to Buy and Sell Stock with Transaction Fee**: Each transaction (buy/sell) incurs a fixed fee. How do you maximize profit after accounting for fees?

4. **Best Time to Buy and Sell Stock III**: You are allowed to complete at most two transactions.

5. **Best Time to Buy and Sell Stock IV**: You are allowed to complete at most k transactions.

6. **Maximum Profit of Operating a Centennial Wheel**: This involves planning operations for a mechanical wheel considering costs and returns over time to maximize profit.

7. **Gas Station Problem**: You need to complete a circuit of gas stations with specified amounts of gas and costs of travel. Determine if you can complete the circuit and return to the starting station.

8. **Trapping Rain Water**: Determine how much rainwater can be trapped between non-negative integers representing the heights of bars.

9. **House Robber**: Given amounts of money in houses arranged linearly, determine the maximum amount of money that can be robbed without robbing two adjacent houses.

10. **House Robber II**: An extension of the House Robber problem where houses are arranged in a circle.

These problems cover various scenarios involving optimization and dynamic programming, providing a solid foundation for tackling interview questions efficiently. They help develop skills in recognizing patterns and leveraging efficient data structures and algorithms to solve complex problems.