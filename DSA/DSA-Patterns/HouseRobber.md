## House Robber - I

### Theory:
- The goal is to maximize the amount of money you can rob from houses, ensuring no two adjacent houses are robbed.
- This is a classic dynamic programming problem.
- We maintain two variables, `a` and `b`, where `a` represents the maximum money that can be robbed up to the previous house (excluding the current house), and `b` represents the maximum money that can be robbed up to the current house.
- For each house, we decide whether to rob it or not, updating our variables accordingly.

```java []
class Solution {
    public int rob(int[] nums) {
        int a = 0, b = 0;
        for(int x: nums) {
            int c = Math.max(a + x, b);
            a = b;
            b = c;
        }
        return b;
    }
}
```

## Paint House

### Theory:
- The goal is to minimize the total cost of painting houses, ensuring no two adjacent houses have the same color.
- This is another dynamic programming problem.
- We maintain variables for the costs of painting the previous house in each of the three colors (a0, b0, c0).
- For each house, we calculate the new costs based on the previous house's costs, ensuring no two adjacent houses have the same color.

```java []
class Solution {
    public int minCost(int[][] costs) {
        int a0 = 0, b0 = 0, c0 = 0;
        for(int[] cost: costs) {
            int a1 = Math.min(b0, c0) + cost[0];
            int b1 = Math.min(a0, c0) + cost[1];
            int c1 = Math.min(a0, b0) + cost[2];
            a0 = a1; b0 = b1; c0 = c1;
        }
        return Math.min(Math.min(a0, b0), c0);
    }
}
```

## House Robber - II

### Theory:
- This problem is a variation of House Robber I where houses are arranged in a circle.
- We cannot rob the first and last house together.
- We solve this by breaking it into two separate problems: one excluding the first house, and the other excluding the last house.

```java []
class Solution {
    public int houseRobberI(int[] nums, int start, int end) {
        int a = 0, b = 0;
        for(int i=start;i<end;i++) {
            int c = Math.max(a + nums[i], b);
            a = b;
            b = c;
        }
        return b;
    }

    public int rob(int[] nums) {
        int n = nums.length;
        if(n == 1) return nums[0];
        return Math.max(
            houseRobberI(nums, 0, n-1),
            houseRobberI(nums, 1, n)
        );
    }
}
```

## Paint House - II

### Theory:
- This problem is a variation of Paint House where there are k colors to choose from.
- We need to minimize the cost of painting houses such that no two adjacent houses have the same color.
- Instead of using a priority queue, we can use two variables min and secondMin to keep track of the minimum and second minimum costs of painting the previous house.
- For each house, we update the minimum and second minimum costs based on the costs of painting the previous house with different colors.

```java []
class Solution {
    public int minCostII(int[][] costs) {
        int k = costs[0].length;
        PriorityQueue<int[]> pq = new PriorityQueue<>((a, b) -> a[0] - b[0]);
        for (int i = 0; i < k; i++) {
            pq.add(new int[]{0, i});
        }
        for (int[] cost : costs) {
            PriorityQueue<int[]> tempPQ = new PriorityQueue<>((a, b) -> a[0] - b[0]);
            for (int i = 0; i < k; i++) {
                int[] peek = pq.peek();
                if (peek[1] == i) {
                    int[] poll = pq.poll();
                    int[] secondPeek = pq.peek();
                    pq.add(poll);
                    peek = secondPeek;
                }
                int[] newData = new int[]{peek[0] + cost[i], i};
                tempPQ.add(newData);
            }
            pq = tempPQ;
        }
        return pq.poll()[0];
    }
}
```


> If you clearly understand this problem, it is similar to Paint House.
But here we will pick minimum among rest of the k-1 colors.
we just care about minimum, but with one cache and that is the same color should not have
minimum, what if the same color has minimum then we will pick second minimum. 
So instead of priority queue, 
>> Can you use just two variables min and secondMin to approach this problem ?

```java []
class Solution {
    public int minCostII(int[][] costs) {
        int n = costs.length;
        if (n == 0) return 0;
        int k = costs[0].length;
        
        int min1 = 0, min2 = 0;
        int minIndex = -1;
        for (int i = 0; i < n; i++) {
            int currentMin1 = Integer.MAX_VALUE, currentMin2 = Integer.MAX_VALUE;
            int currentMinIndex = -1;
            for (int j = 0; j < k; j++) {
                int cost = costs[i][j] + (j == minIndex ? min2 : min1);
                if (cost < currentMin1) {
                    currentMin2 = currentMin1;
                    currentMin1 = cost;
                    currentMinIndex = j;
                } else if (cost < currentMin2) {
                    currentMin2 = cost;
                }
            }
            min1 = currentMin1;
            min2 = currentMin2;
            minIndex = currentMinIndex;
        }

        return min1;
    }
}
```

# 1220. Count Vowels Permutation

If you understand this, then you are good for this type of patterns.

Here we have 5 stats ( a, e, i, o, u ).

```java
class Solution {
    public int countVowelPermutation(int n) {
        long mod = (long) 1e9 + 7;
        long a = 1, e = 1, i = 1, o = 1, u = 1;
        for(int k=1;k<n;k++) {
            long newA = ( ( e%mod + u%mod ) % mod + i%mod ) % mod;
            long newE = ( a%mod + i%mod ) % mod;
            long newI = ( e%mod + o%mod ) % mod;
            long newO = i;
            long newU = ( o%mod + i%mod ) % mod;
            a = newA;
            e = newE;
            i = newI;
            o = newO;
            u = newU;
        }
        return (int)((a + e + i + o + u) % mod);
    }
}
```