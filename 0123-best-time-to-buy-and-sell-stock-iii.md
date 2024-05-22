### Interviewer & Interviewee Discussion

**Interviewer:** Let's discuss a problem where you're given an array `prices` where `prices[i]` is the price of a given stock on day `i`. You need to find the maximum profit you can achieve. However, you are allowed to complete at most two transactions. Note that you can't engage in multiple transactions simultaneously (i.e., you must sell the stock before you buy again).

**Interviewee:** Okay, so if I understand correctly, we can only perform up to two buy-and-sell transactions, and we need to maximize the profit from these transactions.

**Interviewer:** Exactly. Why don't you start by discussing a brute force approach and its complexities?

### Initial Thoughts on Brute Force Approach

**Interviewee:** Sure. One straightforward approach is to try all possible pairs of buy and sell days for the two transactions. Essentially, we can:

1. Try every possible subarray splitting point.
2. For each split:
   - Calculate maximum profit in the first half.
   - Calculate maximum profit in the second half.
3. Sum these profits to get the maximum possible profit for this particular split.

**Interviewer:** That sounds like a reasonable start. What time complexity does this brute-force approach have?

**Interviewee:** Let's break it down:

- To find maximum profit for the first transaction, we would iterate through all pairs of days `(i, j)` where `i < j`. This is O(n^2).
- Similarly, we do the same for the second transaction, also O(n^2).
- Finally, we need to try all possible split points, which is O(n).

So, the overall time complexity would be O(n^3), which isn't feasible for large arrays given the constraint \(1 \leq \text{prices.length} \leq 10^5\).

**Interviewer:** That's correct. O(n^3) is indeed too slow. Can you think of a way to optimize it?

### Optimized Approach Using Dynamic Programming

**Interviewee:** Yes, we can improve his significantly with the help of dynamic programming. We can break this problem into two passes over the prices array to keep track of the best profit we can make from one transaction forward and backward.

1. **First Pass (Forward):**
   - Compute `profit[i]`: maximum profit to be made up to day `i`.

2. **Second Pass (Backward):**
   - Compute the maximum profit with a second transaction starting from day `i`.

**Interviewer:** Can you outline these steps more formally?

**Interviewee:** Absolutely.

**First Pass:**
- We'll maintain two variables, `min_price` to track the minimum price up to day `i` and `max_profit` to track the maximum profit we can make up to day `i`.

```python
min_price = float('inf')
profit = [0] * len(prices)

for i in range(len(prices)):
    min_price = min(min_price, prices[i])
    profit[i] = max(profit[i - 1], prices[i] - min_price)
```

**Second Pass:**
- We'll maintain two variables, `max_price` to track the maximum price from day `i` and `total_max_profit` to track the maximum profit including a second transaction.

```python
max_price = float('-inf')
total_max_profit = 0

for i in range(len(prices) - 1, -1, -1):
    max_price = max(max_price, prices[i])
    total_max_profit = max(total_max_profit, max_price - prices[i] + profit[i])
```

The answer will be in `total_max_profit`.

### Time and Space Complexity

- **Time Complexity:** O(n) for both passes, so overall O(n).
- **Space Complexity:** O(n) for the `profit` array.

### Diagrammatic Explanation

Let's illustrate this with a concrete example:

Given `prices = [3, 3, 5, 0, 0, 3, 1, 4]`:

1. **First Pass:**
   - Calculate profit[0...n-1]

| Day | prices | min_price | profit |
|-----|--------|-----------|--------|
| 0   | 3      | 3         | 0      |
| 1   | 3      | 3         | 0      |
| 2   | 5      | 3         | 2      |
| 3   | 0      | 0         | 2      |
| 4   | 0      | 0         | 2      |
| 5   | 3      | 0         | 3      |
| 6   | 1      | 0         | 3      |
| 7   | 4      | 0         | 4      |

2. **Second Pass:**
   - Calculate total_max_profit

| Day | prices | max_price | Profit + Second Transaction Profit | total_max_profit |
|-----|--------|-----------|-----------------------------------|------------------|
| 7   | 4      | 4         | 4 + 0 = 4                         | 4                |
| 6   | 1      | 4         | 3 + 3 = 6                         | 6                |
| 5   | 3      | 4         | 3 + 1 = 4                         | 6                |
| 4   | 0      | 4         | 2 + 4 = 6                         | 6                |
| 3   | 0      | 4         | 2 + 4 = 6                         | 6                |
| 2   | 5      | 5         | 2 + 0 = 2                         | 6                |
| 1   | 3      | 5         | 0 + 2 = 2                         | 6                |
| 0   | 3      | 5         | 0 + 2 = 2                         | 6                |

Thus, the maximum profit we can make with at most two transactions is 6.

**Interviewer:** That’s a great solution! You’ve clearly explained the logic and the implementation. Good job.
Here is the solution in the various programming languages provided, with the proper time and space complexity mentioned:

**Time Complexity:** O(n)  
**Space Complexity:** O(n)

### C++
```cpp
class Solution {
public:
    int maxProfit(vector<int>& prices) {
        if (prices.empty()) return 0;
        int n = prices.size();
        vector<int> profit(n, 0);

        int min_price = prices[0];
        for (int i = 1; i < n; ++i) {
            min_price = min(min_price, prices[i]);
            profit[i] = max(profit[i - 1], prices[i] - min_price);
        }

        int max_price = prices[n - 1];
        int total_max_profit = 0;
        for (int i = n - 2; i >= 0; --i) {
            max_price = max(max_price, prices[i]);
            total_max_profit = max(total_max_profit, max_price - prices[i] + profit[i]);
        }

        return total_max_profit;
    }
};
```

### Java
```java
class Solution {
    public int maxProfit(int[] prices) {
        if (prices.length == 0) return 0;
        int n = prices.length;
        int[] profit = new int[n];

        int minPrice = prices[0];
        for (int i = 1; i < n; ++i) {
            minPrice = Math.min(minPrice, prices[i]);
            profit[i] = Math.max(profit[i - 1], prices[i] - minPrice);
        }

        int maxPrice = prices[n - 1];
        int totalMaxProfit = 0;
        for (int i = n - 2; i >= 0; --i) {
            maxPrice = Math.max(maxPrice, prices[i]);
            totalMaxProfit = Math.max(totalMaxProfit, maxPrice - prices[i] + profit[i]);
        }

        return totalMaxProfit;
    }
}
```

### Python
```python
class Solution(object):
    def maxProfit(self, prices):
        """
        :type prices: List[int]
        :rtype: int
        """
        if not prices:
            return 0
        
        n = len(prices)
        profit = [0] * n
        
        min_price = prices[0]
        for i in range(1, n):
            min_price = min(min_price, prices[i])
            profit[i] = max(profit[i - 1], prices[i] - min_price)
        
        max_price = prices[n - 1]
        total_max_profit = 0
        for i in range(n - 2, -1, -1):
            max_price = max(max_price, prices[i])
            total_max_profit = max(total_max_profit, max_price - prices[i] + profit[i])
        
        return total_max_profit
```

### Python3
```python
class Solution:
    def maxProfit(self, prices: List[int]) -> int:
        if not prices:
            return 0
        
        n = len(prices)
        profit = [0] * n
        
        min_price = prices[0]
        for i in range(1, n):
            min_price = min(min_price, prices[i])
            profit[i] = max(profit[i - 1], prices[i] - min_price)
        
        max_price = prices[n - 1]
        total_max_profit = 0
        for i in range(n - 2, -1, -1):
            max_price = max(max_price, prices[i])
            total_max_profit = max(total_max_profit, max_price - prices[i] + profit[i])
        
        return total_max_profit
```

### C
```c
int maxProfit(int* prices, int pricesSize) {
    if (pricesSize == 0) return 0;
    int* profit = (int*)malloc(pricesSize * sizeof(int));

    int min_price = prices[0];
    for (int i = 1; i < pricesSize; ++i) {
        min_price = fmin(min_price, prices[i]);
        profit[i] = fmax(profit[i - 1], prices[i] - min_price);
    }

    int max_price = prices[pricesSize - 1];
    int total_max_profit = 0;
    for (int i = pricesSize - 2; i >= 0; --i) {
        max_price = fmax(max_price, prices[i]);
        total_max_profit = fmax(total_max_profit, max_price - prices[i] + profit[i]);
    }

    free(profit);
    return total_max_profit;
}
```

### C#
```csharp
public class Solution {
    public int MaxProfit(int[] prices) {
        if (prices.Length == 0) return 0;
        int n = prices.Length;
        int[] profit = new int[n];

        int minPrice = prices[0];
        for (int i = 1; i < n; ++i) {
            minPrice = Math.Min(minPrice, prices[i]);
            profit[i] = Math.Max(profit[i - 1], prices[i] - minPrice);
        }

        int maxPrice = prices[n - 1];
        int totalMaxProfit = 0;
        for (int i = n - 2; i >= 0; --i) {
            maxPrice = Math.Max(maxPrice, prices[i]);
            totalMaxProfit = Math.Max(totalMaxProfit, maxPrice - prices[i] + profit[i]);
        }

        return totalMaxProfit;
    }
}
```

### JavaScript
```javascript
/**
 * @param {number[]} prices
 * @return {number}
 */
var maxProfit = function(prices) {
    if (prices.length === 0) return 0;
    let n = prices.length;
    let profit = new Array(n).fill(0);

    let minPrice = prices[0];
    for (let i = 1; i < n; ++i) {
        minPrice = Math.min(minPrice, prices[i]);
        profit[i] = Math.max(profit[i - 1], prices[i] - minPrice);
    }

    let maxPrice = prices[n - 1];
    let totalMaxProfit = 0;
    for (let i = n - 2; i >= 0; --i) {
        maxPrice = Math.max(maxPrice, prices[i]);
        totalMaxProfit = Math.max(totalMaxProfit, maxPrice - prices[i] + profit[i]);
    }

    return totalMaxProfit;
};
```

### TypeScript
```typescript
function maxProfit(prices: number[]): number {
    if (prices.length === 0) return 0;
    let n = prices.length;
    let profit = new Array(n).fill(0);

    let minPrice = prices[0];
    for (let i = 1; i < n; ++i) {
        minPrice = Math.min(minPrice, prices[i]);
        profit[i] = Math.max(profit[i - 1], prices[i] - minPrice);
    }

    let maxPrice = prices[n - 1];
    let totalMaxProfit = 0;
    for (let i = n - 2; i >= 0; --i) {
        maxPrice = Math.max(maxPrice, prices[i]);
        totalMaxProfit = Math.max(totalMaxProfit, maxPrice - prices[i] + profit[i]);
    }

    return totalMaxProfit;
};
```

### PHP
```php
class Solution {

    /**
     * @param Integer[] $prices
     * @return Integer
     */
    function maxProfit($prices) {
        if (empty($prices)) return 0;
        $n = count($prices);
        $profit = array_fill(0, $n, 0);

        $min_price = $prices[0];
        for ($i = 1; $i < $n; ++$i) {
            $min_price = min($min_price, $prices[$i]);
            $profit[$i] = max($profit[$i - 1], $prices[$i] - $min_price);
        }

        $max_price = $prices[$n - 1];
        $total_max_profit = 0;
        for ($i = $n - 2; $i >= 0; --$i) {
            $max_price = max($max_price, $prices[$i]);
            $total_max_profit = max($total_max_profit, $max_price - $prices[$i] + $profit[$i]);
        }

        return $total_max_profit;
    }
}
```

### Swift
```swift
class Solution {
    func maxProfit(_ prices: [Int]) -> Int {
        if prices.isEmpty { return 0 }
        let n = prices.count
        var profit = Array(repeating: 0, count: n)

        var minPrice = prices[0]
        for i in 1..<n {
            minPrice = min(minPrice, prices[i])
            profit[i] = max(profit[i - 1], prices[i] - minPrice)
        }

        var maxPrice = prices[n - 1]
        var totalMaxProfit = 0
        for i in (0..<n-1).reversed() {
            maxPrice = max(maxPrice, prices[i])
            totalMaxProfit = max(totalMaxProfit, maxPrice - prices[i] + profit[i])
        }

        return totalMaxProfit
    }
}
```

### Kotlin
```kotlin
class Solution {
    fun maxProfit(prices: IntArray): Int {
        if (prices.isEmpty()) return 0
        val n = prices.size
        val profit = IntArray(n)

        var minPrice = prices[0]
        for (i in 1 until n) {
            minPrice = minOf(minPrice, prices[i])
            profit[i] = maxOf(profit[i - 1], prices[i] - minPrice)
        }

        var maxPrice = prices[n - 1]
        var totalMaxProfit = 0
        for (i in n - 2 downTo 0) {
            maxPrice = maxOf(maxPrice, prices[i])
            totalMaxProfit = maxOf(totalMaxProfit, maxPrice - prices[i] + profit[i])
        }

        return totalMaxProfit
    }
}
```

### Dart
```dart
class Solution {
  int maxProfit(List<int> prices) {
    if (prices.isEmpty) return 0;
    int n = prices.length;
    List<int> profit = List.filled(n, 0);

    int minPrice = prices[0];
    for (int i = 1; i < n; ++i) {
      minPrice = min(minPrice, prices[i]);
      profit[i] = max(profit[i - 1], prices[i] - minPrice);
    }

    int maxPrice = prices[n - 1];
    int totalMaxProfit = 0;
    for (int i = n - 2; i >= 0; --i) {
      maxPrice = max(maxPrice, prices[i]);
      totalMaxProfit = max(totalMaxProfit, maxPrice - prices[i] + profit[i]);
    }

    return totalMaxProfit;
  }
}
```

### Go
```go
func maxProfit(prices []int) int {
    if len(prices) == 0 {
        return 0
    }
    n := len(prices)
    profit := make([]int, n)

    minPrice := prices[0]
    for i := 1; i < n; i++ {
        minPrice = min(minPrice, prices[i])
        profit[i] = max(profit[i-1], prices[i]-minPrice)
    }

    maxPrice := prices[n-1]
    totalMaxProfit := 0
    for i := n - 2; i >= 0; i-- {
        maxPrice = max(maxPrice, prices[i])
        totalMaxProfit = max(totalMaxProfit, maxPrice-prices[i]+profit[i])
    }

    return totalMaxProfit
}

func min(a, b int) int {
    if a < b {
        return a
    }
    return b
}

func max(a, b int) int {
    if a > b {
        return a
    }
    return b
}
```

### Ruby
```ruby
# @param {Integer[]} prices
# @return {Integer}
def max_profit(prices)
    return 0 if prices.empty?
    n = prices.size
    profit = Array.new(n, 0)

    min_price = prices[0]
    for i in 1...n
        min_price = [min_price, prices[i]].min
        profit[i] = [profit[i - 1], prices[i] - min_price].max
    end

    max_price = prices[n - 1]
    total_max_profit = 0
    (n - 2).downto(0) do |i|
        max_price = [max_price, prices[i]].max
        total_max_profit = [total_max_profit, max_price - prices[i] + profit[i]].max
    end

    total_max_profit
end
```

### Scala
```scala
object Solution {
    def maxProfit(prices: Array[Int]): Int = {
        if (prices.isEmpty) return 0
        val n = prices.length
        val profit = Array.fill(n)(0)

        var minPrice = prices(0)
        for (i <- 1 until n) {
            minPrice = math.min(minPrice, prices(i))
            profit(i) = math.max(profit(i - 1), prices(i) - minPrice)
        }

        var maxPrice = prices(n - 1)
        var totalMaxProfit = 0
        for (i <- (0 until n - 1).reverse) {
            maxPrice = math.max(maxPrice, prices(i))
            totalMaxProfit = math.max(totalMaxProfit, maxPrice - prices(i) + profit(i))
        }

        totalMaxProfit
    }
}
```

### Rust
```rust
impl Solution {
    pub fn max_profit(prices: Vec<i32>) -> i32 {
        if prices.is_empty() { return 0; }
        let n = prices.len();
        let mut profit = vec![0; n];

        let mut min_price = prices[0];
        for i in 1..n {
            min_price = std::cmp::min(min_price, prices[i]);
            profit[i] = std::cmp::max(profit[i - 1], prices[i] - min_price);
        }

        let mut max_price = prices[n - 1];
        let mut total_max_profit = 0;
        for i in (0..n-1).rev() {
            max_price = std::cmp::max(max_price, prices[i]);
            total_max_profit = std::cmp::max(total_max_profit, max_price - prices[i] + profit[i]);
        }

        total_max_profit
    }
}
```

### Racket
```racket
(define/contract (max-profit prices)
  (-> (listof exact-integer?) exact-integer?)
  (if (null? prices) 0
      (let* 
          ([n (length prices)]
           [profit (make-vector n 0)]
           [min_price (car prices)])

        (for ([i (in-range 1 n)])
          (set! min_price (min min_price (list-ref prices i)))
          (vector-set! profit i (max (vector-ref profit (- i 1)) 
                                     (- (list-ref prices i) min_price))))
        
        (let ([max_price (list-ref prices (- n 1))]
              [total_max_profit 0])
          
          (for ([i (in-range (- n 2) -1 -1)])
            (set! max_price (max max_price (list-ref prices i)))
            (set! total_max_profit (max total_max_profit 
                                        (+ (- max_price (list-ref prices i)) 
                                           (vector-ref profit i)))))
          total_max_profit))))       
```

### Erlang
```erlang
-spec max_profit(Prices :: [integer()]) -> integer().
max_profit(Prices) ->
    case Prices of
        [] -> 0;
        _ ->
            Profit = lists:duplicate(length(Prices), 0),
            MinPrice = hd(Prices),
            MaxProfit = lists:foldl(fun min_profit/2, {MinPrice, Profit}, tl(Prices)),
            lists:foldl(fun max_profit/2, {MaxProfit, 0}, lists:reverse(RestPrices))
    end.

min_profit(Price, {MinPrice, Profit}) ->
    NewMinPrice = min(MinPrice, Price),
    NewProfit = lists:zipwith(fun(P, M) -> max(P, M - NewMinPrice) end, Profit, [Price]),
    {NewMinPrice, NewProfit}.

max_profit(Price, {MaxProfit, TotalMaxProfit}) ->
    NewMaxPrice = max(MaxProfit, Price),
    NewTotalMaxProfit = max(TotalMaxProfit, NewMaxPrice - Price),
    {NewMaxPrice, NewTotalMaxProfit}.
```

### Elixir
```elixir
defmodule Solution do
  @spec max_profit(prices :: [integer]) :: integer
  def max_profit(prices) do
    if prices == [] do
      0
    else
      n = length(prices)
      profit = List.duplicate(0, n)

      {min_price, profit} = 
        Enum.reduce(1..(n-1), {hd(prices), profit}, fn i, {min_price, profit} ->
          min_price = Enum.min([min_price, Enum.at(prices, i)])
          List.update_at(profit, i, &Enum.max([&1, Enum.at(prices, i) - min_price]))
        end)
      
      {total_max_profit, _, _} = 
        Enum.reduce((n-2)..0, {0, hd(prices), profit}, fn i, {total_max_profit, max_price, profit} ->
          max_price = Enum.max([max_price, Enum.at(prices, i)])
          total_max_profit = Enum.max([total_max_profit, max_price - Enum.at(prices, i) + Enum.at(profit, i)])
          {total_max_profit, max_price, profit}
        end)
      
      total_max_profit
    end
  end
end
```


### Closing Statement

**Interviewer:** You've done an excellent job walking through the problem. You started with a clear understanding of the brute force approach, identified its inefficiency, and then successfully introduced an optimized O(n) solution using dynamic programming. This approach was well-explained and you implemented it across multiple programming languages, illustrating your versatility and attention to detail. 

Your dynamic programming solution efficiently calculates the maximum profit with up to two transactions, maintaining a balance between simplicity and performance. Overall, impressive work!

**Interviewee:** Thank you! This was a challenging yet interesting problem. It's rewarding to see how breaking down the problem and analyzing it step-by-step can lead to a highly efficient solution. I appreciate the productive discussion.

### Similar Questions

Here are some similar questions that require a similar understanding of dynamic programming and greedy algorithms in the context of stock trading:

1. **Best Time to Buy and Sell Stock**:
   - Given an array prices where prices[i] is the price of a given stock on the ith day, find the maximum profit achievable. You may complete only one transaction (i.e., buy one and sell one share).

2. **Best Time to Buy and Sell Stock II**:
   - Similar to the above, but you can complete as many transactions as you like (i.e., buy one and sell one share multiple times).

3. **Best Time to Buy and Sell Stock III**:
   - As solved earlier, you may complete at most two transactions.

4. **Best Time to Buy and Sell Stock IV**:
   - This extends to k transactions where k is given as input, requiring a layer of dynamic programming with an additional dimension.

5. **Best Time to Buy and Sell Stock with Cooldown**:
   - You can't buy stock on the day immediately following a sell (i.e., cooldown period of one day after selling).

6. **Best Time to Buy and Sell Stock with Transaction Fee**:
   - Each transaction involves a fixed fee, which adds complexity in deciding when to make transactions.

Solving these questions will help improve your skills in dynamic programming, greedy algorithms, and problem-solving strategies in competitive programming contexts.