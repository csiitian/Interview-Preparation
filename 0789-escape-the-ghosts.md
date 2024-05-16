**Interviewer:** Welcome! Today, we'll discuss a problem involving a simplified PAC-MAN game on an infinite 2-D grid. Let's walk through it together. You start at the origin, \([0, 0]\), and you need to reach a specified target point \([x_{\text{target}}, y_{\text{target}}]\). There are ghosts on the grid starting at various positions given by a list of coordinates. Each turn, you and the ghosts can move 1 unit in any of the four cardinal directions (north, east, south, or west) or stay still. You escape if you reach the target before any ghost reaches you. If a ghost reaches the target at the same time as you do, you do not escape. Given the ghost positions and the target, can you determine if it is possible to escape?

**Interviewee:** Got it. So, for each ghost and myself, we need to calculate how many steps it takes to reach the target. If I can reach the target before any ghost, I win; otherwise, I lose.

**Interviewer:** Exactly. How would you approach solving this problem?

### Brute Force Approach

**Interviewee:**
1. **Calculate the Manhattan Distance:** The Manhattan distance from \((x_1, y_1)\) to \((x_2, y_2)\) can be calculated as \(|x_1 - x_2| + |y_1 - y_2|\). This is because moves are constrained to the four cardinal directions.
2. For each ghost, calculate the Manhattan distance from its position to the target.
3. Also, calculate my Manhattan distance to the target from \([0, 0]\).
4. If my distance is less than the distance of every ghost to the target, return `true`, else return `false`.

**Interviewer:** That sounds like a good initial approach. Do you want to discuss the time and space complexity of this brute force method?

**Interviewee:**
1. **Time Complexity:** Calculating the Manhattan distance is \(O(1)\). If there are \(n\) ghosts, we need to calculate the distance for each, so the overall time complexity is \(O(n)\).
2. **Space Complexity:** We are only storing a few integer values for comparisons, so the space complexity is \(O(1)\).

**Interviewer:** That’s correct. Given that we need to optimize, do you think this approach is already optimal?

**Interviewee:** Given the constraints (with up to 100 ghosts), the approach seems efficient. However, since calculating the Manhattan distance is straightforward and involves minimal computation, we shouldn't need further optimization. In fact, the approach can't be significantly simplified.

**Interviewer:** Indeed. Why don’t you implement this approach, and we can discuss it further with an example?

### Implementation and Example

```python
def escapeGhosts(ghosts, target):
    my_distance = abs(target[0]) + abs(target[1])
    
    for ghost in ghosts:
        ghost_distance = abs(ghost[0] - target[0]) + abs(ghost[1] - target[1])
        if ghost_distance <= my_distance:
            return False
    return True
```

**Interviewer:** Can you explain with an example?

**Interviewee:**
- Let's take `ghosts = [[1,0],[0,3]]` and `target = [0,1]`.
  - My distance from `[0,0]` to `[0,1]` is `|0-0| + |0-1| = 1`.
  - Distances for ghosts:
    - Ghost 1 at `[1,0]` to `[0,1]`: `|1-0| + |0-1| = 2`.
    - Ghost 2 at `[0,3]` to `[0,1]`: `|0-0| + |3-1| = 2`.
  - Since both ghosts take more steps than I do to reach `[0,1]`, the output is `True`.
  
- Let's take `ghosts = [[1,0]]` and `target = [2,0]`.
  - My distance from `[0,0]` to `[2,0]` is `|0-2| + |0-0| = 2`.
  - Distances for ghosts:
    - Ghost 1 at `[1,0]` to `[2,0]`: `|1-2| + |0-0| = 1`.
  - Since the ghost can reach `[2,0]` before I do, the output is `False`.

**Interviewer:** Great! Your solution and explanation are both clear. Any closing thoughts?

**Interviewee:** This problem demonstrates well the use of Manhattan distances in grid-based pathfinding and illustrates how simultaneous actions factor into such problems.

**Interviewer:** Excellent work. Thank you!

### Visualization

Let's depict the example: `ghosts = [[1,0],[0,3]]` and `target = [0,1]`.

```
Grid Representation:
G = Ghost
P = PAC-MAN (You)
T = Target

  3 (0,3 G)
  2
  1 P(T)
  0 (1,0 G) -1-2-3-4
     0  1  2  3  4 

My Path (P):
  P -> T: One step up (0,1)
Ghost 1 (G at 1,0):
  Move options: 1->0, 1->1
Ghost 2 (G at 0,3):
  Move options: 2->0, 3->2

As shown, Paths are calculated based on Manhattan Distance and I can reach the target before any Ghost.
```
Sure, let's write the solution for each provided language. The core logic for checking if you can escape the ghosts will be the same: comparing your Manhattan distance to the target against each ghost's Manhattan distance to the target.

### C++

```cpp
class Solution {
public:
    bool escapeGhosts(vector<vector<int>>& ghosts, vector<int>& target) {
        int my_distance = abs(target[0]) + abs(target[1]);
        for (const auto& ghost : ghosts) {
            int ghost_distance = abs(ghost[0] - target[0]) + abs(ghost[1] - target[1]);
            if (ghost_distance <= my_distance) {
                return false;
            }
        }
        return true;
    }
};

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Java

```java
class Solution {
    public boolean escapeGhosts(int[][] ghosts, int[] target) {
        int myDistance = Math.abs(target[0]) + Math.abs(target[1]);
        for (int[] ghost : ghosts) {
            int ghostDistance = Math.abs(ghost[0] - target[0]) + Math.abs(ghost[1] - target[1]);
            if (ghostDistance <= myDistance) {
                return false;
            }
        }
        return true;
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Python

```python
class Solution(object):
    def escapeGhosts(self, ghosts, target):
        """
        :type ghosts: List[List[int]]
        :type target: List[int]
        :rtype: bool
        """
        my_distance = abs(target[0]) + abs(target[1])
        for ghost in ghosts:
            ghost_distance = abs(ghost[0] - target[0]) + abs(ghost[1] - target[1])
            if ghost_distance <= my_distance:
                return False
        return True

# Time Complexity: O(n)
# Space Complexity: O(1)
```

### Python3

```python
class Solution:
    def escapeGhosts(self, ghosts: List[List[int]], target: List[int]) -> bool:
        my_distance = abs(target[0]) + abs(target[1])
        for ghost in ghosts:
            ghost_distance = abs(ghost[0] - target[0]) + abs(ghost[1] - target[1])
            if ghost_distance <= my_distance:
                return False
        return True

# Time Complexity: O(n)
# Space Complexity: O(1)
```

### C

```c
bool escapeGhosts(int** ghosts, int ghostsSize, int* ghostsColSize, int* target, int targetSize) {
    int my_distance = abs(target[0]) + abs(target[1]);
    for (int i = 0; i < ghostsSize; ++i) {
        int ghost_distance = abs(ghosts[i][0] - target[0]) + abs(ghosts[i][1] - target[1]);
        if (ghost_distance <= my_distance) {
            return false;
        }
    }
    return true;
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### C#

```csharp
public class Solution {
    public bool EscapeGhosts(int[][] ghosts, int[] target) {
        int myDistance = Math.Abs(target[0]) + Math.Abs(target[1]);
        foreach (var ghost in ghosts) {
            int ghostDistance = Math.Abs(ghost[0] - target[0]) + Math.Abs(ghost[1] - target[1]);
            if (ghostDistance <= myDistance) {
                return false;
            }
        }
        return true;
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### JavaScript

```javascript
/**
 * @param {number[][]} ghosts
 * @param {number[]} target
 * @return {boolean}
 */
var escapeGhosts = function(ghosts, target) {
    let myDistance = Math.abs(target[0]) + Math.abs(target[1]);
    for (let ghost of ghosts) {
        let ghostDistance = Math.abs(ghost[0] - target[0]) + Math.abs(ghost[1] - target[1]);
        if (ghostDistance <= myDistance) {
            return false;
        }
    }
    return true;
};

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### TypeScript

```typescript
function escapeGhosts(ghosts: number[][], target: number[]): boolean {
    const myDistance = Math.abs(target[0]) + Math.abs(target[1]);
    for (const ghost of ghosts) {
        const ghostDistance = Math.abs(ghost[0] - target[0]) + Math.abs(ghost[1] - target[1]);
        if (ghostDistance <= myDistance) {
            return false;
        }
    }
    return true;
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### PHP

```php
class Solution {

    /**
     * @param Integer[][] $ghosts
     * @param Integer[] $target
     * @return Boolean
     */
    function escapeGhosts($ghosts, $target) {
        $my_distance = abs($target[0]) + abs($target[1]);
        foreach ($ghosts as $ghost) {
            $ghost_distance = abs($ghost[0] - $target[0]) + abs($ghost[1] - $target[1]);
            if ($ghost_distance <= $my_distance) {
                return false;
            }
        }
        return true;
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Swift

```swift
class Solution {
    func escapeGhosts(_ ghosts: [[Int]], _ target: [Int]) -> Bool {
        let my_distance = abs(target[0]) + abs(target[1])
        for ghost in ghosts {
            let ghost_distance = abs(ghost[0] - target[0]) + abs(ghost[1] - target[1])
            if ghost_distance <= my_distance {
                return false
            }
        }
        return true
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Kotlin

```kotlin
class Solution {
    fun escapeGhosts(ghosts: Array<IntArray>, target: IntArray): Boolean {
        val myDistance = Math.abs(target[0]) + Math.abs(target[1])
        for (ghost in ghosts) {
            val ghostDistance = Math.abs(ghost[0] - target[0]) + Math.abs(ghost[1] - target[1])
            if (ghostDistance <= myDistance) {
                return false
            }
        }
        return true
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Dart

```dart
class Solution {
  bool escapeGhosts(List<List<int>> ghosts, List<int> target) {
    int myDistance = (target[0]).abs() + (target[1]).abs();
    for (List<int> ghost in ghosts) {
      int ghostDistance = (ghost[0] - target[0]).abs() + (ghost[1] - target[1]).abs();
      if (ghostDistance <= myDistance) {
        return false;
      }
    }
    return true;
  }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Go

```go
func escapeGhosts(ghosts [][]int, target []int) bool {
    myDistance := abs(target[0]) + abs(target[1])
    for _, ghost := range ghosts {
        ghostDistance := abs(ghost[0] - target[0]) + abs(ghost[1] - target[1])
        if ghostDistance <= myDistance {
            return false
        }
    }
    return true
}

func abs(x int) int {
    if x < 0 {
        return -x
    }
    return x
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Ruby

```ruby
# @param {Integer[][]} ghosts
# @param {Integer[]} target
# @return {Boolean}
def escape_ghosts(ghosts, target)
    my_distance = target[0].abs + target[1].abs
    ghosts.each do |ghost|
        ghost_distance = (ghost[0] - target[0]).abs + (ghost[1] - target[1]).abs
        return false if ghost_distance <= my_distance
    end
    true
end

# Time Complexity: O(n)
# Space Complexity: O(1)
```

### Scala

```scala
object Solution {
    def escapeGhosts(ghosts: Array[Array[Int]], target: Array[Int]): Boolean = {
        val myDistance = Math.abs(target(0)) + Math.abs(target(1))
        for (ghost <- ghosts) {
            val ghostDistance = Math.abs(ghost(0) - target(0)) + Math.abs(ghost(1) - target(1))
            if (ghostDistance <= myDistance) return false
        }
        true
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Rust

```rust
impl Solution {
    pub fn escape_ghosts(ghosts: Vec<Vec<i32>>, target: Vec<i32>) -> bool {
        let my_distance = i32::abs(target[0]) + i32::abs(target[1]);
        for ghost in ghosts {
            let ghost_distance = i32::abs(ghost[0] - target[0]) + i32::abs(ghost[1] - target[1]);
            if ghost_distance <= my_distance {
                return false;
            }
        }
        true
    }
}

// Time Complexity: O(n)
// Space Complexity: O(1)
```

### Racket

```racket
(define/contract (escape-ghosts ghosts target)
  (-> (listof (listof exact-integer?)) (listof exact-integer?) boolean?)
  (let ([my-distance (+ (abs (car target)) (abs (cadr target)))])
    (for/or ([ghost ghosts])
      (<= (+ (abs (- (car ghost) (car target)))
             (abs (- (cadr ghost) (cadr target))))
          my-distance))))

; Time Complexity: O(n)
; Space Complexity: O(1)
```

### Erlang

```erlang
-spec escape_ghosts(Ghosts :: [[integer()]], Target :: [integer()]) -> boolean().
escape_ghosts(Ghosts, Target) ->
    MyDist = abs(hd(Target)) + abs(lists:nth(2, Target)),
    lists:all(fun ([GX, GY]) ->
                      GhostDist = abs(GX - hd(Target)) + abs(GY - lists:nth(2, Target)),
                      GhostDist > MyDist
              end, Ghosts).

% Time Complexity: O(n)
% Space Complexity: O(1)
```

### Elixir

```elixir
defmodule Solution do
  @spec escape_ghosts(ghosts :: [[integer]], target :: [integer]) :: boolean
  def escape_ghosts(ghosts, target) do
    my_distance = abs(Enum.at(target, 0)) + abs(Enum.at(target, 1))
    Enum.all?(ghosts, fn ghost ->
      ghost_distance = abs(Enum.at(ghost, 0) - Enum.at(target, 0)) + abs(Enum.at(ghost, 1) - Enum.at(target, 1))
      ghost_distance > my_distance
    end)
  end
end

# Time Complexity: O(n)
# Space Complexity: O(1)
```

These code snippets solve the given problem using the same logic and ensure readability and efficiency with the given language constraints.


### Closing Statement

In this discussion, we explored an intriguing problem inspired by the PAC-MAN game, where a player must navigate an infinite 2-D grid to reach a target while avoiding ghosts. We developed a solution based on the Manhattan distance, recognizing that the optimal escape route hinges on the player reaching the target before any ghost. After crafting the initial brute-force solution, we validated its efficiency and translated it into various programming languages, ensuring that it maintains an optimal time complexity of \(O(n)\) and space complexity of \(O(1)\). This exercise not only reinforced our understanding of distance calculations in grid-based problems but also demonstrated the importance of efficient translation across multiple programming paradigms.

### Similar Questions

To further hone your skills, here are some similar problems:

1. **Robot Bounded In Circle**:
   Given a list of instructions ('G' for go straight, 'L' for turn left, 'R' for turn right), determine if the robot bounded in a circle.

2. **Unique Paths**:
   A robot is located at the top-left corner of a \(m \times n\) grid. The robot can only move down or right at any point in time. Find the number of unique paths to reach the bottom-right corner.

3. **Minimum Knight Moves**:
   Find the minimum number of steps for a knight in an infinite chessboard to reach a target position starting from a source position.

4. **Flood Fill**:
   Implement the "flood fill" algorithm in an image represented by a 2D array, starting from a given pixel and changing its color to the new given color.

5. **Shortest Path in Binary Matrix**:
   Given a binary matrix, find the shortest path from the top-left corner to the bottom-right corner, moving in 8 directions.

6. **Pac-Man Mazes**:
   Design an algorithm that navigates a Pac-Man from a start position to a goal in a maze, while collecting as many points as possible without being caught by ghosts.

7. **Game of Life**:
   Implement the "Game of Life", which is a cellular automaton where you determine the next state of a grid based on its current state and the state of its neighbors.

8. **Rotting Oranges**:
   In a matrix of oranges, determine the minimum time required for all fresh oranges to become rotten given that rotting spreads to neighboring fresh oranges each minute.

These problems share similar underlying principles such as grid traversal, distance calculation, and optimization strategies, thus offering a great way to continue practicing and refining your algorithmic skills. Happy coding!