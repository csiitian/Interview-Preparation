### Interviewer and Interviewee Discussion

**Interviewer**: Let's tackle the problem of converting an Excel column title to its corresponding column number. For example, given "A," it should return 1, and given "AB," it should return 28. How would you approach this problem initially?

**Interviewee**: To start, we need to recognize that this problem involves converting a string to a number, somewhat similar to converting a number in base-26 to a decimal number. Each letter in the string can be viewed as a digit in a base-26 number system where 'A' corresponds to 1 and 'Z' corresponds to 26.

**Interviewer**: Exactly. So, what would be your brute force approach?

**Interviewee**: For a brute force approach, I would:
1. Iterate through each character of the string.
2. Convert each character to its corresponding number using its position in the alphabet.
3. Use a running total that considers the place value of each character in the string.

To map the character to its corresponding number, I can use `ord(character) - ord('A') + 1`.

### Brute Force Approach

**Interviewee**: Let's break this down with an example. Consider "AB":
- 'A' maps to 1.
- 'B' maps to 2.
- The column number for "AB" can be calculated as \(1 \times 26^1 + 2 \times 26^0\).

```python
def titleToNumber(columnTitle: str) -> int:
    result = 0
    for char in columnTitle:
        result = result * 26 + (ord(char) - ord('A') + 1)
    return result
```

**Interviewer**: Great, let's discuss the time and space complexities of this approach.

### Time and Space Complexity

**Interviewee**:
- **Time Complexity**: We are iterating once through the string of length `n`, so the time complexity is \(O(n)\).
- **Space Complexity**: We are using a constant amount of extra space regardless of the input size, so the space complexity is \(O(1)\).

### Visualization

**Interviewee**: Let's visualize the conversion of "AB" to its corresponding column number.

- "AB" = \(1 \times 26^1 + 2 \times 26^0\):
   - Compute for 'A': \(1 \times 26 \rightarrow 26\)
   - Add 'B': \(26 + 2 = 28\)

**Illustration:**

For "AB":
```
     'A', 'B'
     ↓    ↓
26^1  26^0
  ↓    ↓
  1    2
 ↓     ↓
1*26^1 + 2*26^0 = 26 + 2 = 28
```

**Interviewer**: This makes perfect sense. Your approach is efficient and easy to understand. Excellent work!

**Interviewee**: Thank you!
Sure, I will provide the solution in the given methods for each language. Let's implement the `titleToNumber` function:

### C++
```cpp
class Solution {
public:
    int titleToNumber(string columnTitle) {
        int result = 0;
        for (char c : columnTitle) {
            result = result * 26 + (c - 'A' + 1);
        }
        return result;
    }
};
```

### Java
```java
class Solution {
    public int titleToNumber(String columnTitle) {
        int result = 0;
        for (char c : columnTitle.toCharArray()) {
            result = result * 26 + (c - 'A' + 1);
        }
        return result;
    }
}
```

### Python
```python
class Solution(object):
    def titleToNumber(self, columnTitle):
        """
        :type columnTitle: str
        :rtype: int
        """
        result = 0
        for char in columnTitle:
            result = result * 26 + (ord(char) - ord('A') + 1)
        return result
```

### Python3
```python
class Solution:
    def titleToNumber(self, columnTitle: str) -> int:
        result = 0
        for char in columnTitle:
            result = result * 26 + (ord(char) - ord('A') + 1)
        return result
```

### C
```c
int titleToNumber(char* columnTitle) {
    int result = 0;
    while (*columnTitle) {
        result = result * 26 + (*columnTitle - 'A' + 1);
        columnTitle++;
    }
    return result;
}
```

### C#
```csharp
public class Solution {
    public int TitleToNumber(string columnTitle) {
        int result = 0;
        foreach (char c in columnTitle) {
            result = result * 26 + (c - 'A' + 1);
        }
        return result;
    }
}
```

### JavaScript
```javascript
/**
 * @param {string} columnTitle
 * @return {number}
 */
var titleToNumber = function(columnTitle) {
    let result = 0;
    for (let i = 0; i < columnTitle.length; i++) {
        result = result * 26 + (columnTitle.charCodeAt(i) - 'A'.charCodeAt(0) + 1);
    }
    return result;
};
```

### TypeScript
```typescript
function titleToNumber(columnTitle: string): number {
    let result = 0;
    for (let i = 0; i < columnTitle.length; i++) {
        result = result * 26 + (columnTitle.charCodeAt(i) - 'A'.charCodeAt(0) + 1);
    }
    return result;
}
```

### PHP
```php
class Solution {

    /**
     * @param String $columnTitle
     * @return Integer
     */
    function titleToNumber($columnTitle) {
        $result = 0;
        for ($i = 0; $i < strlen($columnTitle); $i++) {
            $result = $result * 26 + (ord($columnTitle[$i]) - ord('A') + 1);
        }
        return $result;
    }
}
```

### Swift
```swift
class Solution {
    func titleToNumber(_ columnTitle: String) -> Int {
        var result = 0
        for c in columnTitle {
            result = result * 26 + Int(c.asciiValue! - Character("A").asciiValue! + 1)
        }
        return result
    }
}
```

### Kotlin
```kotlin
class Solution {
    fun titleToNumber(columnTitle: String): Int {
        var result = 0
        for (c in columnTitle) {
            result = result * 26 + (c - 'A' + 1)
        }
        return result
    }
}
```

### Dart
```dart
class Solution {
  int titleToNumber(String columnTitle) {
    int result = 0;
    for (int i = 0; i < columnTitle.length; i++) {
      result = result * 26 + (columnTitle.codeUnitAt(i) - 'A'.codeUnitAt(0) + 1);
    }
    return result;
  }
}
```

### Go
```go
func titleToNumber(columnTitle string) int {
    result := 0
    for _, c := range columnTitle {
        result = result * 26 + int(c - 'A' + 1)
    }
    return result
}
```

### Ruby
```ruby
# @param {String} column_title
# @return {Integer}
def title_to_number(column_title)
    result = 0
    column_title.each_char do |c|
        result = result * 26 + (c.ord - 'A'.ord + 1)
    end
    result
end
```

### Scala
```scala
object Solution {
    def titleToNumber(columnTitle: String): Int = {
        var result = 0
        for (c <- columnTitle) {
            result = result * 26 + (c - 'A' + 1)
        }
        result
    }
}
```

### Rust
```rust
impl Solution {
    pub fn title_to_number(column_title: String) -> i32 {
        let mut result = 0;
        for c in column_title.chars() {
            result = result * 26 + (c as i32 - 'A' as i32 + 1);
        }
        result
    }
}
```

### Racket
```racket
(define/contract (title-to-number columnTitle)
  (-> string? exact-integer?)
  (define result 0)
  (for ([c (string->list columnTitle)])
    (set! result (+ (* result 26) (- (char->integer c) (char->integer #\A) 1))))
  result
)
```

### Erlang
```erlang
-spec title_to_number(ColumnTitle :: unicode:unicode_binary()) -> integer().
title_to_number(ColumnTitle) ->
    title_to_number(ColumnTitle, 0).

title_to_number(<<>>, Result) ->
    Result;
title_to_number(<<H, T/binary>>, Result) ->
    NewResult = Result * 26 + (H - $A + 1),
    title_to_number(T, NewResult).
```

### Elixir
```elixir
defmodule Solution do
  @spec title_to_number(column_title :: String.t) :: integer
  def title_to_number(column_title) do
    column_title
    |> String.to_charlist()
    |> Enum.reduce(0, fn char, acc -> acc * 26 + (char - ?A + 1) end)
  end
end
```


### Closing Statement

**Interviewer**: Great job! You've successfully implemented the solution in multiple programming languages with clear and optimized code. You understood the problem well, and your approach to converting the Excel column title into its corresponding column number was both efficient and effective.

**Interviewee**: Thank you! It was interesting to see the similarity to base-26 numeral systems, and I enjoyed implementing the solution across different languages. The discussion on the complexities assured me that this approach is well-optimized.

**Interviewer**: Indeed, your solution has \(O(n)\) time complexity and \(O(1)\) space complexity, which are quite optimal for this problem. Now let's look at similar problems to practice further:

### Similar Questions

1. **Excel Sheet Column Title (LeetCode #168)**
   - **Description**: Given a positive integer, return its corresponding column title as appears in an Excel sheet. For example, 1 -> A, 2 -> B, 26 -> Z, 27 -> AA, etc.
   - **Tags**: String, Math

2. **Integer to Roman (LeetCode #12)**
   - **Description**: Convert an integer to a Roman numeral. Input is guaranteed to be within the range from 1 to 3999.
   - **Tags**: String, Hash Table, Math

3. **Roman to Integer (LeetCode #13)**
   - **Description**: Convert a Roman numeral to an integer.
   - **Tags**: String, Hash Table

4. **Base 7 (LeetCode #504)**
   - **Description**: Given an integer, return its base 7 string representation.
   - **Tags**: Math

5. **Find the nth Digit (LeetCode #400)**
   - **Description**: Find the nth digit of the infinite integer sequence: 1, 2, 3, 4, 5, ..., 11, 12, ....
   - **Tags**: Math

### Resources for Further Study

To get better at these types of problems, you can study:
- Conversions between different numeral systems.
- String manipulation techniques.
- Basic combinatorial mathematics principles.

---

**Interviewer**: Once again, great work today! With more practice on similar problems, you'll have a strong grip on these kinds of number-to-string and string-to-number conversion challenges.

**Interviewee**: Thank you! I appreciate the feedback and look forward to practicing more.