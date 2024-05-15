## Interviewer and Interviewee Discussion

### Initial Discussion

**Interviewer**: 
Let's discuss a problem on finding unique paths for a robot on a grid. The robot starts at the top-left corner of an m x n grid and can move only right or down. Some cells contain obstacles that the robot cannot pass through. We need to determine the number of unique paths the robot can take to reach the bottom-right corner.

**Interviewee**: 
Sure. So, just to clarify, the robot can't move diagonally and has to avoid any obstacles marked by '1', correct?

**Interviewer**:
Correct. Obstacles are marked as '1' and free paths are marked as '0'. For example, given a grid like:
```
[[0, 0, 0],
 [0, 1, 0],
 [0, 0, 0]]
```
The robot has to find the number of unique paths from `grid[0][0]` to `grid[2][2]`.

### Initial Thoughts on Brute Force Approach

**Interviewee**:
The brute force approach would involve exploring all possible paths from the top-left to the bottom-right corner. We could use a recursive method to explore every possible combination of moving right or down, and then count the paths. This would, however, be extremely inefficient because it would involve recomputing the number of paths for many cells multiple times.

**Interviewer**:
Exactly. Do you see any issues with this approach in terms of complexity?

**Interviewee**:
Yes. The time complexity would be exponential because each move could lead to two recursive calls (right or down), resulting in essentially a binary tree of height m + n. This results in a complexity of O(2^(m+n)). The space complexity would be O(m+n) due to the recursion stack.

### Optimizing with a Dynamic Programming Approach

**Interviewer**:
Good. How can we improve on this approach?

**Interviewee**:
We can use dynamic programming to store the results of subproblems so that each subproblem is computed only once. We'll use a 2D DP array where `dp[i][j]` represents the number of unique paths to reach the cell (i, j). Here's the plan:

1. Initialize a 2D DP array with the same dimensions as the grid.
2. If the starting cell (0,0) or the ending cell (m-1, n-1) is an obstacle, return 0 directly, as no path is possible.
3. Set `dp[0][0] = 1` if there is no obstacle at the start.
4. For each cell (i, j):
   - If it's an obstacle, set `dp[i][j] = 0`.
   - Otherwise, set `dp[i][j] = dp[i-1][j] + dp[i][j-1]` if these indices are valid (i.e., within bounds and not obstacles).

This way, we fill the DP table in a bottom-up manner.

**Interviewer**:
That sounds good. Can you explain the details including the time and space complexity?

**Interviewee**:
Sure, here it is:

- **Initialization**: Create a `dp` array of size `m x n` in O(m*n) time.
- **Filling the DP Table**: This also involves iterating through each cell once, leading to O(m*n) time.
- Overall **Time Complexity**: O(m*n)
- **Space Complexity**: The space required for the DP table is also O(m*n).

Here’s a visual representation to explain:

### Visual Explanation

Consider the example grid:
```
[[0, 0, 0],
 [0, 1, 0],
 [0, 0, 0]]
```

- **Initialization**:
  ```
  dp = [[1, 1, 1],
        [1, 0, 0],
        [1, 0, 0]]
  ```

- **Filling the DP Table**:
  - `dp[1][1]` is an obstacle: `dp[1][1] = 0`
  - `dp[1][2] = dp[0][2] + dp[1][1] = 1 + 0 = 1`
  - `dp[2][1] = dp[1][1] + dp[2][0] = 0 + 1 = 1`
  - `dp[2][2] = dp[1][2] + dp[2][1] = 1 + 1 = 2`

Final DP Table:
```
dp = [[1, 1, 1],
      [1, 0, 1],
      [1, 1, 2]]
```

Thus, the number of unique paths is `dp[m-1][n-1] = dp[2][2] = 2`.

By filling the DP table this way, we ensure efficient and correct computation of the number of paths.

**Interviewer**:
Excellent. This dynamic programming approach is efficient in both time and space. Good job!
Certainly! Here is the complete solution implemented in the various requested programming languages. Each solution uses dynamic programming as explained earlier.

### C++

```cpp
class Solution {
public:
    int uniquePathsWithObstacles(vector<vector<int>>& obstacleGrid) {
        int m = obstacleGrid.size();
        int n = obstacleGrid[0].size();
        
        if (obstacleGrid[0][0] == 1 || obstacleGrid[m - 1][n - 1] == 1) {
            return 0;
        }
        
        vector<vector<int>> dp(m, vector<int>(n, 0));
        dp[0][0] = 1;
        
        for (int i = 0; i < m; ++i) {
            for (int j = 0; j < n; ++j) {
                if (obstacleGrid[i][j] == 1) {
                    dp[i][j] = 0;
                } else {
                    if (i > 0) {
                        dp[i][j] += dp[i - 1][j];
                    }
                    if (j > 0) {
                        dp[i][j] += dp[i][j - 1];
                    }
                }
            }
        }
        
        return dp[m - 1][n - 1];
    }
};
```

### Java

```java
class Solution {
    public int uniquePathsWithObstacles(int[][] obstacleGrid) {
        int m = obstacleGrid.length;
        int n = obstacleGrid[0].length;
        
        if (obstacleGrid[0][0] == 1 || obstacleGrid[m - 1][n - 1] == 1) {
            return 0;
        }
        
        int[][] dp = new int[m][n];
        dp[0][0] = 1;
        
        for (int i = 0; i < m; ++i) {
            for (int j = 0; j < n; ++j) {
                if (obstacleGrid[i][j] == 1) {
                    dp[i][j] = 0;
                } else {
                    if (i > 0) {
                        dp[i][j] += dp[i - 1][j];
                    }
                    if (j > 0) {
                        dp[i][j] += dp[i][j - 1];
                    }
                }
            }
        }
        
        return dp[m - 1][n - 1];
    }
}
```

### Python

```python
class Solution(object):
    def uniquePathsWithObstacles(self, obstacleGrid):
        """
        :type obstacleGrid: List[List[int]]
        :rtype: int
        """
        m, n = len(obstacleGrid), len(obstacleGrid[0])
        
        if obstacleGrid[0][0] == 1 or obstacleGrid[m - 1][n - 1] == 1:
            return 0
        
        dp = [[0] * n for _ in range(m)]
        dp[0][0] = 1
        
        for i in range(m):
            for j in range(n):
                if obstacleGrid[i][j] == 1:
                    dp[i][j] = 0
                else:
                    if i > 0:
                        dp[i][j] += dp[i - 1][j]
                    if j > 0:
                        dp[i][j] += dp[i][j - 1]
        
        return dp[m - 1][n - 1]
```

### Python3

```python
class Solution:
    def uniquePathsWithObstacles(self, obstacleGrid: List[List[int]]) -> int:
        m, n = len(obstacleGrid), len(obstacleGrid[0])
        
        if obstacleGrid[0][0] == 1 or obstacleGrid[m - 1][n - 1] == 1:
            return 0
        
        dp = [[0] * n for _ in range(m)]
        dp[0][0] = 1
        
        for i in range(m):
            for j in range(n):
                if obstacleGrid[i][j] == 1:
                    dp[i][j] = 0
                else:
                    if i > 0:
                        dp[i][j] += dp[i - 1][j]
                    if j > 0:
                        dp[i][j] += dp[i][j - 1]
        
        return dp[m - 1][n - 1]
```

### C

```c
int uniquePathsWithObstacles(int** obstacleGrid, int obstacleGridSize, int* obstacleGridColSize) {
    int m = obstacleGridSize;
    int n = obstacleGridColSize[0];
    
    if (obstacleGrid[0][0] == 1 || obstacleGrid[m - 1][n - 1] == 1) {
        return 0;
    }
    
    int dp[m][n];
    memset(dp, 0, sizeof(dp));
    dp[0][0] = 1;
    
    for (int i = 0; i < m; ++i) {
        for (int j = 0; j < n; ++j) {
            if (obstacleGrid[i][j] == 1) {
                dp[i][j] = 0;
            } else {
                if (i > 0) {
                    dp[i][j] += dp[i - 1][j];
                }
                if (j > 0) {
                    dp[i][j] += dp[i][j - 1];
                }
            }
        }
    }
    
    return dp[m - 1][n - 1];
}
```

### C#

```csharp
public class Solution {
    public int UniquePathsWithObstacles(int[][] obstacleGrid) {
        int m = obstacleGrid.Length;
        int n = obstacleGrid[0].Length;

        if (obstacleGrid[0][0] == 1 || obstacleGrid[m - 1][n - 1] == 1) {
            return 0;
        }

        int[,] dp = new int[m, n];
        dp[0, 0] = 1;

        for (int i = 0; i < m; ++i) {
            for (int j = 0; j < n; ++j) {
                if (obstacleGrid[i][j] == 1) {
                    dp[i, j] = 0;
                } else {
                    if (i > 0) {
                        dp[i, j] += dp[i - 1, j];
                    }
                    if (j > 0) {
                        dp[i, j] += dp[i, j - 1];
                    }
                }
            }
        }

        return dp[m - 1, n - 1];
    }
}
```

### JavaScript

```javascript
/**
 * @param {number[][]} obstacleGrid
 * @return {number}
 */
var uniquePathsWithObstacles = function(obstacleGrid) {
    let m = obstacleGrid.length;
    let n = obstacleGrid[0].length;
    
    if (obstacleGrid[0][0] == 1 || obstacleGrid[m - 1][n - 1] == 1) {
        return 0;
    }
    
    let dp = Array(m).fill().map(() => Array(n).fill(0));
    dp[0][0] = 1;
    
    for (let i = 0; i < m; ++i) {
        for (let j = 0; j < n; ++j) {
            if (obstacleGrid[i][j] === 1) {
                dp[i][j] = 0;
            } else {
                if (i > 0) {
                    dp[i][j] += dp[i - 1][j];
                }
                if (j > 0) {
                    dp[i][j] += dp[i][j - 1];
                }
            }
        }
    }
    
    return dp[m - 1][n - 1];
};
```

### TypeScript

```typescript
function uniquePathsWithObstacles(obstacleGrid: number[][]): number {
    let m = obstacleGrid.length;
    let n = obstacleGrid[0].length;

    if (obstacleGrid[0][0] === 1 || obstacleGrid[m - 1][n - 1] === 1) {
        return 0;
    }

    let dp = Array.from({ length: m }, () => Array(n).fill(0));
    dp[0][0] = 1;

    for (let i = 0; i < m; ++i) {
        for (let j = 0; j < n; ++j) {
            if (obstacleGrid[i][j] === 1) {
                dp[i][j] = 0;
            } else {
                if (i > 0) {
                    dp[i][j] += dp[i - 1][j];
                }
                if (j > 0) {
                    dp[i][j] += dp[i][j - 1];
                }
            }
        }
    }

    return dp[m - 1][n - 1];
}
```

### PHP

```php
class Solution {

    /**
     * @param Integer[][] $obstacleGrid
     * @return Integer
     */
    function uniquePathsWithObstacles($obstacleGrid) {
        $m = count($obstacleGrid);
        $n = count($obstacleGrid[0]);

        if ($obstacleGrid[0][0] == 1 || $obstacleGrid[$m - 1][$n - 1] == 1) {
            return 0;
        }

        $dp = array_fill(0, $m, array_fill(0, $n, 0));
        $dp[0][0] = 1;

        for ($i = 0; $i < $m; ++$i) {
            for ($j = 0; $j < $n; ++$j) {
                if ($obstacleGrid[$i][$j] == 1) {
                    $dp[$i][$j] = 0;
                } else {
                    if ($i > 0) {
                        $dp[$i][$j] += $dp[$i - 1][$j];
                    }
                    if ($j > 0) {
                        $dp[$i][$j] += $dp[$i][$j - 1];
                    }
                }
            }
        }

        return $dp[$m - 1][$n - 1];
    }
}
```

### Swift

```swift
class Solution {
    func uniquePathsWithObstacles(_ obstacleGrid: [[Int]]) -> Int {
        let m = obstacleGrid.count
        let n = obstacleGrid[0].count

        if obstacleGrid[0][0] == 1 || obstacleGrid[m - 1][n - 1] == 1 {
            return 0
        }

        var dp = Array(repeating: Array(repeating: 0, count: n), count: m)
        dp[0][0] = 1

        for i in 0..<m {
            for j in 0..<n {
                if obstacleGrid[i][j] == 1 {
                    dp[i][j] = 0
                } else {
                    if i > 0 {
                        dp[i][j] += dp[i - 1][j]
                    }
                    if j > 0 {
                        dp[i][j] += dp[i][j - 1]
                    }
                }
            }
        }

        return dp[m - 1][n - 1]
    }
}
```

### Kotlin

```kotlin
class Solution {
    fun uniquePathsWithObstacles(obstacleGrid: Array<IntArray>): Int {
        val m = obstacleGrid.size
        val n = obstacleGrid[0].size

        if (obstacleGrid[0][0] == 1 || obstacleGrid[m - 1][n - 1] == 1) {
            return 0
        }

        val dp = Array(m) { IntArray(n) }
        dp[0][0] = 1

        for (i in 0 until m) {
            for (j in 0 until n) {
                if (obstacleGrid[i][j] == 1) {
                    dp[i][j] = 0
                } else {
                    if (i > 0) {
                        dp[i][j] += dp[i - 1][j]
                    }
                    if (j > 0) {
                        dp[i][j] += dp[i][j - 1]
                    }
                }
            }
        }

        return dp[m - 1][n - 1]
    }
}
```

### Dart

```dart
class Solution {
  int uniquePathsWithObstacles(List<List<int>> obstacleGrid) {
    int m = obstacleGrid.length;
    int n = obstacleGrid[0].length;

    if (obstacleGrid[0][0] == 1 || obstacleGrid[m - 1][n - 1] == 1) {
      return 0;
    }

    List<List<int>> dp = List.generate(
        m, (i) => List<int>.generate(n, (j) => 0, growable: false),
        growable: false);
    dp[0][0] = 1;

    for (int i = 0; i < m; ++i) {
      for (int j = 0; j < n; ++j) {
        if (obstacleGrid[i][j] == 1) {
          dp[i][j] = 0;
        } else {
          if (i > 0) {
            dp[i][j] += dp[i - 1][j];
          }
          if (j > 0) {
            dp[i][j] += dp[i][j - 1];
          }
        }
      }
    }

    return dp[m - 1][n - 1];
  }
}
```

### Go

```go
func uniquePathsWithObstacles(obstacleGrid [][]int) int {
    m := len(obstacleGrid)
    n := len(obstacleGrid[0])

    if obstacleGrid[0][0] == 1 || obstacleGrid[m-1][n-1] == 1 {
        return 0
    }

    dp := make([][]int, m)
    for i := range dp {
        dp[i] = make([]int, n)
    }
    dp[0][0] = 1

    for i := 0; i < m; i++ {
        for j := 0; j < n; j++ {
            if obstacleGrid[i][j] == 1 {
                dp[i][j] = 0
            } else {
                if i > 0 {
                    dp[i][j] += dp[i-1][j]
                }
                if j > 0 {
                    dp[i][j] += dp[i][j-1]
                }
            }
        }
    }

    return dp[m-1][n-1]
}
```

### Ruby

```ruby
# @param {Integer[][]} obstacle_grid
# @return {Integer}
def unique_paths_with_obstacles(obstacle_grid)
    m = obstacle_grid.length
    n = obstacle_grid[0].length

    if obstacle_grid[0][0] == 1 || obstacle_grid[m-1][n-1] == 1
        return 0
    end

    dp = Array.new(m) { Array.new(n, 0) }
    dp[0][0] = 1

    for i in 0...m
        for j in 0...n
            if obstacle_grid[i][j] == 1
                dp[i][j] = 0
            else
                if i > 0
                    dp[i][j] += dp[i-1][j]
                end
                if j > 0
                    dp[i][j] += dp[i][j-1]
                end
            end
        end
    end

    dp[m-1][n-1]
end
```

### Scala

```scala
object Solution {
    def uniquePathsWithObstacles(obstacleGrid: Array[Array[Int]]): Int = {
        val m = obstacleGrid.length
        val n = obstacleGrid(0).length

        if (obstacleGrid(0)(0) == 1 || obstacleGrid(m-1)(n-1) == 1) {
            return 0
        }

        val dp = Array.fill(m, n)(0)
        dp(0)(0) = 1

        for {
            i <- 0 until m
            j <- 0 until n
        } {
            if (obstacleGrid(i)(j) == 1) {
                dp(i)(j) = 0
            } else {
                if (i > 0) {
                    dp(i)(j) += dp(i - 1)(j)
                }
                if (j > 0) {
                    dp(i)(j) += dp(i)(j - 1)
                }
            }
        }

        dp(m - 1)(n - 1)
    }
}
```

### Rust

```rust
impl Solution {
    pub fn unique_paths_with_obstacles(obstacle_grid: Vec<Vec<i32>>) -> i32 {
        let m = obstacle_grid.len();
        let n = obstacle_grid[0].len();

        if obstacle_grid[0][0] == 1 || obstacle_grid[m - 1][n - 1] == 1 {
            return 0;
        }

        let mut dp = vec![vec![0; n]; m];
        dp[0][0] = 1;

        for i in 0..m {
            for j in 0..n {
                if obstacle_grid[i][j] == 1 {
                    dp[i][j] = 0;
                } else {
                    if i > 0 {
                        dp[i][j] += dp[i - 1][j];
                    }
                    if j > 0 {
                        dp[i][j] += dp[i][j - 1];
                    }
                }
            }
        }

        dp[m - 1][n - 1]
    }
}
```

### Closing Statement

**Interviewer**:
Great job! We discussed an interesting problem involving finding unique paths in a grid with obstacles. You initially proposed a brute force approach and quickly identified its inefficiencies. Then, you moved on to an optimized solution using dynamic programming, explaining both the time and space complexity clearly. Finally, you translated this solution into various programming languages, showing a deep understanding of both the problem and the dynamic programming approach.

**Interviewee**:
Thank you! It was a fascinating problem that really required careful thought regarding optimization and efficiency. I enjoyed exploring the dynamic programming solution and translating it across multiple languages.

### Similar Questions

1. **Unique Paths**: Find the number of unique paths from the top-left corner to the bottom-right corner of a grid, without obstacles. (LeetCode - Unique Paths)

2. **Minimum Path Sum**: Given a grid filled with non-negative numbers, find a path from the top-left corner to the bottom-right corner that minimizes the sum of the numbers along the path. The robot can only move down or right. (LeetCode - Minimum Path Sum)

3. **Coin Change 2**: Given a total amount of money and a list of coin denominations, find the number of ways to make up that amount. (LeetCode - Coin Change 2)

4. **Climbing Stairs**: Given a staircase with `n` steps, determine how many distinct ways there are to climb to the top, where at each step you can climb either 1 or 2 steps. (LeetCode - Climbing Stairs)

5. **Triangle**: Given a triangle array, return the minimum path sum from top to bottom. You can only move to adjacent numbers on the row below. (LeetCode - Triangle)

6. **Jump Game II**: Given an array of non-negative integers where each element represents the maximum jump length from that position, determine the minimum number of jumps required to reach the last index. (LeetCode - Jump Game II)

7. **Longest Increasing Path in a Matrix**: Given a matrix, find the length of the longest increasing path in the matrix. From each cell, you can move in four directions (left, right, up, or down). (LeetCode - Longest Increasing Path in a Matrix)

8. **Dungeon Game**: Given a 2D dungeon grid, determine the minimum initial health needed for a knight to rescue a princess located at the bottom-right corner, starting from the top-left corner. The knight can only move right or down. (LeetCode - Dungeon Game)

These questions will help you build on the concepts of dynamic programming and pathfinding in grids, and each comes with its own unique challenges and variations. Happy coding!