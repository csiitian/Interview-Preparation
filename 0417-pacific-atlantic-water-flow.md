### Interviewer and Interviewee Discussion

**Interviewer:** Let's discuss a problem. You're given an `m x n` rectangular island represented by a grid of heights. The island borders both the Pacific and Atlantic Oceans. The top and left edges are adjacent to the Pacific, and the bottom and right edges are adjacent to the Atlantic. Given the heights of the cells, you need to determine which cells can allow rainwater to flow to both the Pacific and Atlantic oceans. How would you approach this problem?

**Interviewee:** To start with, I'll ensure I understand the problem statement correctly. You're saying that water flows from a higher height to an equal or lower height in the four cardinal directions. My task is to identify all cells from which water can flow to both the Pacific and Atlantic Oceans. Correct?

**Interviewer:** That's right.

### Initial Thoughts on Brute Force Approach

**Interviewee:** For the brute force approach, the simplest idea is to simulate the water flow for each cell to see if it can reach both oceans. Here’s how I can start with the brute force approach:

1. For each cell, perform a breadth-first search (BFS) or depth-first search (DFS) to check if water can reach both the Pacific and Atlantic Oceans.
2. Start the search from each cell and mark those that can flow to the Pacific and Atlantic.

**Interviewer:** That sounds reasonable. What do you think would be the time and space complexity of this brute force approach?

**Interviewee:** 
- **Time Complexity:** For each cell, the BFS/DFS could potentially traverse the entire grid, making it O(m * n) operations per cell. Since there are `m * n` cells, the total time complexity would be O((m * n) * (m * n)).
- **Space Complexity:** We need a visited array of size O(m * n) for each BFS/DFS traversal. So the space complexity would be O(m * n) for the queue and visited array in BFS/DFS.

### Optimizing the Approach

**Interviewee:** This brute force method seems highly inefficient due to its high time complexity. We can optimize this approach. Instead of starting BFS/DFS from each cell, we can:

1. Perform a BFS/DFS starting from the cells adjacent to the Pacific Ocean (top and left edges). Mark all cells that can flow to the Pacific.
2. Perform a BFS/DFS starting from the cells adjacent to the Atlantic Ocean (bottom and right edges). Mark all cells that can flow to the Atlantic.
3. The intersection of the cells that can flow to the Pacific and Atlantic is our result.

**Interviewer:** Interesting. Can you elaborate on how you can implement this and what will be the time complexity of this optimized approach?

**Interviewee:** Sure. Here's a more detailed plan:

1. Create two matrices, `pacificReachable` and `atlanticReachable`, both initialized to false.
2. Perform BFS/DFS for each cell on the Pacific border and fill `pacificReachable`.
3. Perform BFS/DFS for each cell on the Atlantic border and fill `atlanticReachable`.
4. Iterate through the grid and collect cells marked as reachable in both matrices.

The time complexity of this would be:
- **Time Complexity:** Each cell is visited at most twice, once for each ocean. So the complexity is O(m * n).
- **Space Complexity:** We need additional space for the two matrices, so it's O(m * n).

Here's the code implementation for this optimized approach:

```python
def pacificAtlantic(heights):
    if not heights:
        return []
    
    m, n = len(heights), len(heights[0])
    pacific_reachable = [[False] * n for _ in range(m)]
    atlantic_reachable = [[False] * n for _ in range(m)]
    
    def dfs(matrix, x, y, prev_height):
        if (x < 0 or x >= m or y < 0 or y >= n or
                matrix[x][y] or heights[x][y] < prev_height):
            return
        matrix[x][y] = True
        for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
            dfs(matrix, x + dx, y + dy, heights[x][y])
    
    for i in range(m):
        dfs(pacific_reachable, i, 0, heights[i][0])
        dfs(atlantic_reachable, i, n - 1, heights[i][n - 1])

    for j in range(n):
        dfs(pacific_reachable, 0, j, heights[0][j])
        dfs(atlantic_reachable, m - 1, j, heights[m - 1][j])

    result = []
    for i in range(m):
        for j in range(n):
            if pacific_reachable[i][j] and atlantic_reachable[i][j]:
                result.append([i, j])
    
    return result
```

### Visualization

To better understand this, let’s visualize a simplified example:

Consider the grid:  
```
heights = [
    [1, 2, 2],
    [3, 2, 1],
    [1, 1, 2]
]
```

1. **Pacific Reachable** after BFS/DFS from top and left edges:
```
true  true  true  
true  false false 
true  true  true  
```

2. **Atlantic Reachable** after BFS/DFS from bottom and right edges:
```
false false true  
true  false true  
true  true  true  
```

3. **Intersection** of above matrices gives cells that can reach both oceans:
```
true  false true
true  false false
true  true  true  
```

So, the result is `[[0, 0], [0, 2], [1, 0], [2, 0], [2, 1], [2, 2]]`.

**Interviewer:** Great! This optimized approach is clear, efficient, and well-explained. Good job!
Sure! Let's translate the optimized approach to all the given programming languages with the specified method signatures.

### C++

```cpp
class Solution {
public:
    vector<vector<int>> pacificAtlantic(vector<vector<int>>& heights) {
        if (heights.empty()) return {};
        int m = heights.size(), n = heights[0].size();
        vector<vector<bool>> pacificReachable(m, vector<bool>(n, false));
        vector<vector<bool>> atlanticReachable(m, vector<bool>(n, false));
        
        function<void(int, int, vector<vector<bool>>&)> dfs = [&](int x, int y, vector<vector<bool>>& reachable) {
            if (reachable[x][y]) return;
            reachable[x][y] = true;
            vector<int> dir = {0, 1, 0, -1, 0};
            for (int i = 0; i < 4; ++i) {
                int nx = x + dir[i], ny = y + dir[i + 1];
                if (nx >= 0 && nx < m && ny >= 0 && ny < n && heights[nx][ny] >= heights[x][y]) {
                    dfs(nx, ny, reachable);
                }
            }
        };
        
        for (int i = 0; i < m; ++i) {
            dfs(i, 0, pacificReachable);
            dfs(i, n - 1, atlanticReachable);
        }
        for (int j = 0; j < n; ++j) {
            dfs(0, j, pacificReachable);
            dfs(m - 1, j, atlanticReachable);
        }
        
        vector<vector<int>> result;
        for (int i = 0; i < m; ++i) {
            for (int j = 0; j < n; ++j) {
                if (pacificReachable[i][j] && atlanticReachable[i][j]) {
                    result.push_back({i, j});
                }
            }
        }
        return result;
    }
};
```

### Java

```java
import java.util.ArrayList;
import java.util.List;

class Solution {
    public List<List<Integer>> pacificAtlantic(int[][] heights) {
        if (heights.length == 0) return new ArrayList<>();
        int m = heights.length, n = heights[0].length;
        boolean[][] pacificReachable = new boolean[m][n];
        boolean[][] atlanticReachable = new boolean[m][n];
        
        for (int i = 0; i < m; i++) {
            dfs(heights, pacificReachable, Integer.MIN_VALUE, i, 0);
            dfs(heights, atlanticReachable, Integer.MIN_VALUE, i, n - 1);
        }
        for (int j = 0; j < n; j++) {
            dfs(heights, pacificReachable, Integer.MIN_VALUE, 0, j);
            dfs(heights, atlanticReachable, Integer.MIN_VALUE, m - 1, j);
        }
        
        List<List<Integer>> result = new ArrayList<>();
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                if (pacificReachable[i][j] && atlanticReachable[i][j]) {
                    result.add(List.of(i, j));
                }
            }
        }
        return result;
    }
    
    private void dfs(int[][] heights, boolean[][] reachable, int prevHeight, int x, int y) {
        if (x < 0 || x >= heights.length || y < 0 || y >= heights[0].length || reachable[x][y] || heights[x][y] < prevHeight) {
            return;
        }
        reachable[x][y] = true;
        dfs(heights, reachable, heights[x][y], x + 1, y);
        dfs(heights, reachable, heights[x][y], x - 1, y);
        dfs(heights, reachable, heights[x][y], x, y + 1);
        dfs(heights, reachable, heights[x][y], x, y - 1);
    }
}
```

### Python

```python
class Solution(object):
    def pacificAtlantic(self, heights):
        """
        :type heights: List[List[int]]
        :rtype: List[List[int]]
        """
        if not heights:
            return []
        
        m, n = len(heights), len(heights[0])
        pacific_reachable = [[False] * n for _ in range(m)]
        atlantic_reachable = [[False] * n for _ in range(m)]
        
        def dfs(matrix, x, y, prev_height):
            if x < 0 or x >= m or y < 0 or y >= n or matrix[x][y] or heights[x][y] < prev_height:
                return
            matrix[x][y] = True
            for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                dfs(matrix, x + dx, y + dy, heights[x][y])
        
        for i in range(m):
            dfs(pacific_reachable, i, 0, heights[i][0])
            dfs(atlantic_reachable, i, n - 1, heights[i][n - 1])

        for j in range(n):
            dfs(pacific_reachable, 0, j, heights[0][j])
            dfs(atlantic_reachable, m - 1, j, heights[m - 1][j])

        result = []
        for i in range(m):
            for j in range(n):
                if pacific_reachable[i][j] and atlantic_reachable[i][j]:
                    result.append([i, j])
        
        return result
```

### Python3

```python
class Solution:
    def pacificAtlantic(self, heights: List[List[int]]) -> List[List[int]]:
        if not heights:
            return []
        
        m, n = len(heights), len(heights[0])
        pacific_reachable = [[False] * n for _ in range(m)]
        atlantic_reachable = [[False] * n for _ in range(m)]
        
        def dfs(matrix, x, y, prev_height):
            if x < 0 or x >= m or y < 0 or y >= n or matrix[x][y] or heights[x][y] < prev_height:
                return
            matrix[x][y] = True
            for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                dfs(matrix, x + dx, y + dy, heights[x][y])
        
        for i in range(m):
            dfs(pacific_reachable, i, 0, heights[i][0])
            dfs(atlantic_reachable, i, n - 1, heights[i][n - 1])

        for j in range(n):
            dfs(pacific_reachable, 0, j, heights[0][j])
            dfs(atlantic_reachable, m - 1, j, heights[m - 1][j])

        result = []
        for i in range(m):
            for j in range(n):
                if pacific_reachable[i][j] and atlantic_reachable[i][j]:
                    result.append([i, j])
        
        return result
```

### C

```c
#include <stdlib.h>

void dfs(int** heights, int** reach, int heightsSize, int heightsColSize, int x, int y, int currHeight) {
    if (x < 0 || y < 0 || x >= heightsSize || y >= heightsColSize || reach[x][y] || heights[x][y] < currHeight) {
        return;
    }
    reach[x][y] = 1;
    dfs(heights, reach, heightsSize, heightsColSize, x + 1, y, heights[x][y]);
    dfs(heights, reach, heightsSize, heightsColSize, x - 1, y, heights[x][y]);
    dfs(heights, reach, heightsSize, heightsColSize, x, y + 1, heights[x][y]);
    dfs(heights, reach, heightsSize, heightsColSize, x, y - 1, heights[x][y]);
}

/**
 * Return an array of arrays of size *returnSize.
 * The sizes of the arrays are returned as *returnColumnSizes array.
 * Note: Both returned array and *columnSizes array must be malloced, assume caller calls free().
 */
int** pacificAtlantic(int** heights, int heightsSize, int* heightsColSize, int* returnSize, int** returnColumnSizes) {
    if (heightsSize == 0) {
        *returnSize = 0;
        *returnColumnSizes = NULL;
        return NULL;
    }
    int** res = (int**)malloc(heightsSize * heightsColSize[0] * sizeof(int*));
    *returnColumnSizes = (int*)malloc(heightsSize * heightsColSize[0] * sizeof(int));
    int pacific[heightsSize][*heightsColSize];
    int atlantic[heightsSize][*heightsColSize];
    memset(pacific, 0, sizeof(pacific));
    memset(atlantic, 0, sizeof(atlantic));

    for (int i = 0; i < heightsSize; i++) {
        dfs(heights, (int**) pacific, heightsSize, *heightsColSize, i, 0, heights[i][0]);
        dfs(heights, (int**) atlantic, heightsSize, *heightsColSize, i, *heightsColSize - 1, heights[i][*heightsColSize - 1]);
    }
    for (int j = 0; j < *heightsColSize; j++) {
        dfs(heights, (int**) pacific, heightsSize, *heightsColSize, 0, j, heights[0][j]);
        dfs(heights, (int**) atlantic, heightsSize, *heightsColSize, heightsSize - 1, j, heights[heightsSize - 1][j]);
    }

    *returnSize = 0;
    for (int i = 0; i < heightsSize; i++) {
        for (int j = 0; j < heightsColSize[0]; j++) {
            if (pacific[i][j] && atlantic[i][j]) {
                res[*returnSize] = (int*)malloc(2 * sizeof(int));
                res[*returnSize][0] = i;
                res[*returnSize][1] = j;
                (*returnColumnSizes)[*returnSize] = 2;
                (*returnSize)++;
            }
        }
    }
    return res;
}
```

### C#

```csharp
using System.Collections.Generic;

public class Solution {
    public IList<IList<int>> PacificAtlantic(int[][] heights) {
        if (heights == null || heights.Length == 0) return new List<IList<int>>();
        int m = heights.Length, n = heights[0].Length;
        bool[,] pacificReachable = new bool[m, n];
        bool[,] atlanticReachable = new bool[m, n];
        
        for (int i = 0; i < m; i++) {
            Dfs(heights, pacificReachable, int.MinValue, i, 0);
            Dfs(heights, atlanticReachable, int.MinValue, i, n - 1);
        }
        for (int j = 0; j < n; j++) {
            Dfs(heights, pacificReachable, int.MinValue, 0, j);
            Dfs(heights, atlanticReachable, int.MinValue, m - 1, j);
        }
        
        IList<IList<int>> result = new List<IList<int>>();
        for (int i = 0; i < m; i++) {
            for (int j = 0; j < n; j++) {
                if (pacificReachable[i, j] && atlanticReachable[i, j]) {
                    result.Add(new List<int> { i, j });
                }
            }
        }
        return result;
    }
    
    private void Dfs(int[][] heights, bool[,] reachable, int prevHeight, int x, int y) {
        if (x < 0 || y < 0 || x >= heights.Length || y >= heights[0].Length || reachable[x, y] || heights[x][y] < prevHeight) {
            return;
        }
        reachable[x, y] = true;
        int[] dx = { -1, 1, 0, 0 };
        int[] dy = { 0, 0, -1, 1 };
        for (int i = 0; i < 4; i++) {
            Dfs(heights, reachable, heights[x][y], x + dx[i], y + dy[i]);
        }
    }
}
```

### JavaScript

```javascript
/**
 * @param {number[][]} heights
 * @return {number[][]}
 */
var pacificAtlantic = function(heights) {
    if (heights.length === 0) return [];
    const m = heights.length, n = heights[0].length;
    const pacificReachable = Array.from({ length: m }, () => Array(n).fill(false));
    const atlanticReachable = Array.from({ length: m }, () => Array(n).fill(false));
    
    for (let i = 0; i < m; i++) {
        dfs(heights, pacificReachable, Number.MIN_SAFE_INTEGER, i, 0);
        dfs(heights, atlanticReachable, Number.MIN_SAFE_INTEGER, i, n - 1);
    }
    for (let j = 0; j < n; j++) {
        dfs(heights, pacificReachable, Number.MIN_SAFE_INTEGER, 0, j);
        dfs(heights, atlanticReachable, Number.MIN_SAFE_INTEGER, m - 1, j);
    }
    
    const result = [];
    for (let i = 0; i < m; i++) {
        for (let j = 0; j < n; j++) {
            if (pacificReachable[i][j] && atlanticReachable[i][j]) {
                result.push([i, j]);
            }
        }
    }
    return result;
};

function dfs(heights, reachable, prevHeight, x, y) {
    if (x < 0 || x >= heights.length || y < 0 || y >= heights[0].length || reachable[x][y] || heights[x][y] < prevHeight) {
        return;
    }
    reachable[x][y] = true;
    const dx = [-1, 1, 0, 0];
    const dy = [0, 0, -1, 1];
    for (let i = 0; i < 4; i++) {
        dfs(heights, reachable, heights[x][y], x + dx[i], y + dy[i]);
    }
}
```

### TypeScript

```typescript
function pacificAtlantic(heights: number[][]): number[][] {
    if (heights.length === 0) return [];
    const m = heights.length, n = heights[0].length;
    const pacificReachable = Array.from({ length: m }, () => Array(n).fill(false));
    const atlanticReachable = Array.from({ length: m }, () => Array(n).fill(false));
    
    for (let i = 0; i < m; i++) {
        dfs(heights, pacificReachable, Number.MIN_SAFE_INTEGER, i, 0);
        dfs(heights, atlanticReachable, Number.MIN_SAFE_INTEGER, i, n - 1);
    }
    for (let j = 0; j < n; j++) {
        dfs(heights, pacificReachable, Number.MIN_SAFE_INTEGER, 0, j);
        dfs(heights, atlanticReachable, Number.MIN_SAFE_INTEGER, m - 1, j);
    }
    
    const result: number[][] = [];
    for (let i = 0; i < m; i++) {
        for (let j = 0; j < n; j++) {
            if (pacificReachable[i][j] && atlanticReachable[i][j]) {
                result.push([i, j]);
            }
        }
    }
    return result;
};

function dfs(heights: number[][], reachable: boolean[][], prevHeight: number, x: number, y: number) {
    if (x < 0 || x >= heights.length || y < 0 || y >= heights[0].length || reachable[x][y] || heights[x][y] < prevHeight) {
        return;
    }
    reachable[x][y] = true;
    const dx = [-1, 1, 0, 0];
    const dy = [0, 0, -1, 1];
    for (let i = 0; i < 4; i++) {
        dfs(heights, reachable, heights[x][y], x + dx[i], y + dy[i]);
    }
}
```


### Closing Statement

**Interviewee:** Thank you. I hope my solution met the expectations. We discussed the problem thoroughly, starting from the brute force approach to moving onto an optimized solution using DFS from the borders of the Pacific and Atlantic oceans. I implemented the solution in various programming languages as requested. The optimized approach significantly reduces the time complexity from O((m * n) * (m * n)) to O(m * n), making it scalable for larger grids.

**Interviewer:** Excellent! You did a great job breaking down the problem and moving from an initial brute-force approach to a more efficient solution. Your implementation is clean and comprehensive across multiple languages. Keep up the good work! Let's wrap up this session. Before we close, I have some similar questions for you to explore.

### Similar Questions

1. **Number of Islands (Leetcode 200)**
   - **Description:** Given a 2D grid map of '1's (land) and '0's (water), count the number of islands. An island is surrounded by water and is formed by connecting adjacent lands horizontally or vertically.
   
2. **Surrounded Regions (Leetcode 130)**
   - **Description:** Given a 2D board containing 'X' and 'O', capture all regions surrounded by 'X'. A region is captured by flipping all 'O's into 'X's in that surrounded region.

3. **Flood Fill (Leetcode 733)**
   - **Description:** An image is represented by a 2D array of integers, where each integer represents the pixel value. Perform a "flood fill" operation starting from a given pixel and a new color.

4. **Walls and Gates (Leetcode 286)**
   - **Description:** Given a 2D grid filled with gates (0), walls (-1), and empty rooms (INF), fill each empty room with the distance to its nearest gate. If it is impossible to reach a gate, that room remains INF.

5. **Word Search (Leetcode 79)**
   - **Description:** Given a 2D board and a word, find if the word exists in the grid. The word can be constructed from letters of sequentially adjacent cells, where adjacent cells are horizontally or vertically neighboring.

6. **Matrix (Leetcode 542)**
   - **Description:** Given an m x n binary matrix `mat`, return the distance of the nearest 0 for each cell.

Exploring these questions will deepen your understanding of grid-based algorithms and various traversal techniques.

**Interviewer:** Keep practicing with these problems, and you'll continue to improve your problem-solving and coding skills. Have a great day!