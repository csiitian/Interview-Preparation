### Interviewer and Interviewee Discussion

**Interviewer:** Let's discuss the problem where you have a string `s` and an integer `k`. You can change up to `k` characters in the string to any other uppercase English character. Your goal is to find the length of the longest substring that contains only one unique character after performing these changes. How would you approach this problem?

**Interviewee:** First, let's consider a brute force approach. We can check every possible substring, calculate how many changes are required to make all the characters in that substring the same, and keep track of the maximum length of such substrings. 

**Interviewer:** That sounds good. Can you walk me through the brute force solution step by step?

**Interviewee:** Sure. For each starting index `i` of the substring, for each ending index `j`, we can count the frequency of each character in the substring `s[i:j+1]`. If the number of changes needed to make all characters the same is less than or equal to `k`, we update the maximum length.

### Brute Force Approach

**Pseudocode:**
1. Initialize `max_length` to 0.
2. Loop through each starting index `i` from 0 to `len(s)-1`.
3. Loop through each ending index `j` from `i` to `len(s)-1`.
4. Calculate the frequency of each character in the substring `s[i:j+1]`.
5. Find the character with the maximum frequency, let's say it's `max_freq_char`.
6. Calculate the number of changes needed: `len(s[i:j+1]) - frequency of max_freq_char`.
7. If this value is less than or equal to `k`, update `max_length` with the maximum of current `max_length` and the length of the substring `s[i:j+1]`.
8. Return `max_length`.

**Interviewer:** That makes sense. What would be the time complexity of this brute force solution?

**Interviewee:** The brute force solution would have a time complexity of O(n^3) because we are iterating over all possible substrings and for each substring, we are calculating the frequency which takes O(n) time. Therefore, it’s not efficient for large strings.

**Interviewer:** Can you think of a more optimal solution?

**Interviewee:** Yes, we can use a sliding window technique to optimize this solution. By maintaining a window with a fixed starting and ending pointer, and keeping track of the frequency of characters within that window, we can avoid recalculating frequencies from scratch for overlapping parts of the window.

### Optimized Sliding Window Approach with Frequency Map

**Optimized Algorithm:**
1. Initialize two pointers: `left` and `right` both set to 0.
2. Initialize a frequency dictionary to keep track of counts of characters in the current window.
3. Initialize `max_freq` to keep track of the highest frequency of any character in the current window.
4. Iterate `right` from 0 to len(s)-1.
   - Increment the frequency of the character `s[right]`.
   - Update `max_freq` with the maximum frequency in the current window.
   - If the current window size minus `max_freq` is greater than `k`, move the `left` pointer to shrink the window.
5. Keep track of the maximum window size during the iteration.
6. Return the size of the maximum window found.

**Pseudocode:**
```python
def characterReplacement(s: str, k: int) -> int:
    left = 0
    max_length = 0
    max_freq = 0
    freq = {}
    
    for right in range(len(s)):
        freq[s[right]] = freq.get(s[right], 0) + 1
        max_freq = max(max_freq, freq[s[right]])
        
        if (right - left + 1) - max_freq > k:
            freq[s[left]] -= 1
            left += 1
        
        max_length = max(max_length, right - left + 1)
    
    return max_length
```

### Time and Space Complexity

**Time Complexity:**
- The time complexity of this approach is O(n), where `n` is the length of the string. Each character is processed at most twice (once when expanding `right` and at most once more when contracting from the `left`), making it linear time.

**Space Complexity:**
- The space complexity is O(1) because the frequency dictionary will have at most 26 entries (for each letter in the English alphabet).

### Visual Explanation

```plaintext
Sliding Window Explanation:
Example: s = "AABABBA", k = 1

Initial state:
- left = 0, right = 0, max_length = 0, max_freq = 0, freq = {}

Step-by-step window development:
- right = 0 -> freq = {'A': 1}, max_freq = 1, max_length = 1
- right = 1 -> freq = {'A': 2}, max_freq = 2, max_length = 2
- right = 2 -> freq = {'A': 2, 'B': 1}, max_freq = 2, max_length = 3
- right = 3 -> freq = {'A': 2, 'B': 2}, max_freq = 2, max_length = 4
- right = 4 -> freq = {'A': 2, 'B': 3}, max_freq = 3, max_length = 4
- right = 5 -> freq = {'A': 2, 'B': 3}, max_freq = 3, move left to 1
- freq = {'A': 1, 'B': 3}, max_length = 4
- right = 6 -> freq = {'A': 1, 'B': 4}, max_freq = 4, max_length = 4
```

The sliding window technique efficiently finds the longest substring by adjusting the window size dynamically based on the constraints, resulting in a linear time solution.
### C++

```cpp
class Solution {
public:
    int characterReplacement(string s, int k) {
        int left = 0, max_length = 0, max_freq = 0;
        unordered_map<char, int> freq;
        
        for (int right = 0; right < s.size(); ++right) {
            freq[s[right]]++;
            max_freq = max(max_freq, freq[s[right]]);
            
            if ((right - left + 1) - max_freq > k) {
                freq[s[left]]--;
                left++;
            }
            
            max_length = max(max_length, right - left + 1);
        }
        
        return max_length;
    }
};
```

**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Java

```java
class Solution {
    public int characterReplacement(String s, int k) {
        int left = 0, max_length = 0, max_freq = 0;
        int[] freq = new int[26];
        
        for (int right = 0; right < s.length(); ++right) {
            max_freq = Math.max(max_freq, ++freq[s.charAt(right) - 'A']);
            
            if ((right - left + 1) - max_freq > k) {
                freq[s.charAt(left) - 'A']--;
                left++;
            }
            
            max_length = Math.max(max_length, right - left + 1);
        }
        
        return max_length;
    }
}
```

**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Python

```python
class Solution(object):
    def characterReplacement(self, s, k):
        """
        :type s: str
        :type k: int
        :rtype: int
        """
        left = 0
        max_length = 0
        max_freq = 0
        freq = {}
        
        for right in range(len(s)):
            freq[s[right]] = freq.get(s[right], 0) + 1
            max_freq = max(max_freq, freq[s[right]])
            
            if (right - left + 1) - max_freq > k:
                freq[s[left]] -= 1
                left += 1
            
            max_length = max(max_length, right - left + 1)
        
        return max_length
```

**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Python3

```python
class Solution:
    def characterReplacement(self, s: str, k: int) -> int:
        left = 0
        max_length = 0
        max_freq = 0
        freq = {}
        
        for right in range(len(s)):
            freq[s[right]] = freq.get(s[right], 0) + 1
            max_freq = max(max_freq, freq[s[right]])
            
            if (right - left + 1) - max_freq > k:
                freq[s[left]] -= 1
                left += 1
            
            max_length = max(max_length, right - left + 1)
        
        return max_length
```

**Time Complexity:** O(n)
**Space Complexity:** O(1)

### C

```c
int characterReplacement(char* s, int k) {
    int left = 0, max_length = 0, max_freq = 0;
    int freq[26] = {0};
    
    for (int right = 0; s[right] != '\0'; ++right) {
        freq[s[right] - 'A']++;
        if (freq[s[right] - 'A'] > max_freq) {
            max_freq = freq[s[right] - 'A'];
        }
        
        if ((right - left + 1) - max_freq > k) {
            freq[s[left] - 'A']--;
            left++;
        }
        
        if ((right - left + 1) > max_length) {
            max_length = right - left + 1;
        }
    }
    
    return max_length;
}
```

**Time Complexity:** O(n)
**Space Complexity:** O(1)

### C#

```csharp
public class Solution {
    public int CharacterReplacement(string s, int k) {
        int left = 0, max_length = 0, max_freq = 0;
        int[] freq = new int[26];
        
        for (int right = 0; right < s.Length; ++right) {
            max_freq = Math.Max(max_freq, ++freq[s[right] - 'A']);
            
            if ((right - left + 1) - max_freq > k) {
                freq[s[left] - 'A']--;
                left++;
            }
            
            max_length = Math.Max(max_length, right - left + 1);
        }
        
        return max_length;
    }
}
```

**Time Complexity:** O(n)
**Space Complexity:** O(1)

### JavaScript

```javascript
/**
 * @param {string} s
 * @param {number} k
 * @return {number}
 */
var characterReplacement = function(s, k) {
    let left = 0, max_length = 0, max_freq = 0;
    const freq = {};
    
    for (let right = 0; right < s.length; ++right) {
        freq[s[right]] = (freq[s[right]] || 0) + 1;
        max_freq = Math.max(max_freq, freq[s[right]]);
        
        if ((right - left + 1) - max_freq > k) {
            freq[s[left]]--;
            left++;
        }
        
        max_length = Math.max(max_length, right - left + 1);
    }
    
    return max_length;
};
```

**Time Complexity:** O(n)
**Space Complexity:** O(1)

### TypeScript

```typescript
function characterReplacement(s: string, k: number): number {
    let left = 0, max_length = 0, max_freq = 0;
    const freq: { [key: string]: number } = {};
    
    for (let right = 0; right < s.length; ++right) {
        freq[s[right]] = (freq[s[right]] || 0) + 1;
        max_freq = Math.max(max_freq, freq[s[right]]);
        
        if ((right - left + 1) - max_freq > k) {
            freq[s[left]]--;
            left++;
        }
        
        max_length = Math.max(max_length, right - left + 1);
    }
    
    return max_length;
}
```

**Time Complexity:** O(n)
**Space Complexity:** O(1)

### PHP

```php
class Solution {

    /**
     * @param String $s
     * @param Integer $k
     * @return Integer
     */
    function characterReplacement($s, $k) {
        $left = 0;
        $max_length = 0;
        $max_freq = 0;
        $freq = [];
        
        for ($right = 0; $right < strlen($s); ++$right) {
            $freq[$s[$right]] = ($freq[$s[$right]] ?? 0) + 1;
            $max_freq = max($max_freq, $freq[$s[$right]]);
            
            if (($right - $left + 1) - $max_freq > $k) {
                $freq[$s[$left]]--;
                $left++;
            }
            
            $max_length = max($max_length, $right - $left + 1);
        }
        
        return $max_length;
    }
}
```

**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Swift

```swift
class Solution {
    func characterReplacement(_ s: String, _ k: Int) -> Int {
        var left = 0, max_length = 0, max_freq = 0
        var freq = [Character: Int]()
        let sArray = Array(s)
        
        for right in 0..<sArray.count {
            freq[sArray[right], default: 0] += 1
            max_freq = max(max_freq, freq[sArray[right]]!)
            
            if (right - left + 1) - max_freq > k {
                freq[sArray[left]]! -= 1
                left += 1
            }
            
            max_length = max(max_length, right - left + 1)
        }
        
        return max_length
    }
}
```

**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Kotlin

```kotlin
class Solution {
    fun characterReplacement(s: String, k: Int): Int {
        var left = 0
        var max_length = 0
        var max_freq = 0
        val freq = mutableMapOf<Char, Int>()
        
        for (right in s.indices) {
            freq[s[right]] = freq.getOrDefault(s[right], 0) + 1
            max_freq = maxOf(max_freq, freq[s[right]]!!)
            
            if ((right - left + 1) - max_freq > k) {
                freq[s[left]] = freq[s[left]]!! - 1
                left++
            }
            
            max_length = maxOf(max_length, right - left + 1)
        }
        
        return max_length
    }
}
```

**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Dart

```dart
class Solution {
  int characterReplacement(String s, int k) {
    int left = 0;
    int max_length = 0;
    int max_freq = 0;
    Map<String, int> freq = {};
    
    for (int right = 0; right < s.length; ++right) {
      freq[s[right]] = (freq[s[right]] ?? 0) + 1;
      max_freq = max(max_freq, freq[s[right]]!);
      
      if ((right - left + 1) - max_freq > k) {
        freq[s[left]]!--;
        left++;
      }
      
      max_length = max(max_length, right - left + 1);
    }
    
    return max_length;
  }
}

int max(int a, int b) => (a > b) ? a : b;
```

**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Go

```go
func characterReplacement(s string, k int) int {
    left, max_length, max_freq := 0, 0, 0
    freq := make(map[byte]int)
    
    for right := 0; right < len(s); right++ {
        freq[s[right]]++
        if freq[s[right]] > max_freq {
            max_freq = freq[s[right]]
        }
        
        if (right - left + 1) - max_freq > k {
            freq[s[left]]--
            left++
        }
        
        if (right - left + 1) > max_length {
            max_length = right - left + 1
        }
    }
    
    return max_length
}
```

**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Ruby

```ruby
# @param {String} s
# @param {Integer} k
# @return {Integer}
def character_replacement(s, k)
    left, max_length, max_freq = 0, 0, 0
    freq = Hash.new(0)
    
    s.each_char.with_index do |char, right|
        freq[char] += 1
        max_freq = [max_freq, freq[char]].max
        
        if (right - left + 1) - max_freq > k
            freq[s[left]] -= 1
            left += 1
        end
        
        max_length = [max_length, right - left + 1].max
    end
    
    return max_length
end
```

**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Scala

```scala
object Solution {
    def characterReplacement(s: String, k: Int): Int = {
        var left = 0
        var max_length = 0
        var max_freq = 0
        val freq = scala.collection.mutable.Map[Char, Int]()
        
        for (right <- s.indices) {
            freq(s(right)) = freq.getOrElse(s(right), 0) + 1
            max_freq = max_freq.max(freq(s(right)))
            
            if ((right - left + 1) - max_freq > k) {
                freq(s(left)) -= 1
                left += 1
            }
            
            max_length = max_length.max(right - left + 1)
        }
        
        return max_length
    }
}
```

**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Rust

```rust
impl Solution {
    pub fn character_replacement(s: String, k: i32) -> i32 {
        let mut left = 0;
        let mut max_length = 0;
        let mut max_freq = 0;
        let mut freq = std::collections::HashMap::new();
        let bytes = s.into_bytes();
        
        for right in 0..bytes.len() {
            *freq.entry(bytes[right]).or_insert(0) += 1;
            max_freq = max_freq.max(*freq.get(&bytes[right]).unwrap());
            
            if (right - left + 1) as i32 - max_freq as i32 > k {
                *freq.get_mut(&bytes[left]).unwrap() -= 1;
                left += 1;
            }
            
            max_length = max_length.max((right - left + 1) as i32);
        }
        
        return max_length;
    }
}
```

**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Racket

```racket
(define/contract (character-replacement s k)
  (-> string? exact-integer? exact-integer?)

  (define s-len (string-length s))
  (define left 0)
  (define max-length 0)
  (define max-freq 0)
  (define freq (make-hash))

  (for ([right s-len])
    (hash-set! freq (string-ref s right) (+ 1 (hash-ref freq (string-ref s right) 0)))
    (let ([current-freq (hash-ref freq (string-ref s right) 0)])
      (set! max-freq (max max-freq current-freq)))

    (when (> (- (+ (- right left 1) 1) max-freq) k)
      (hash-set! freq (string-ref s left) (- (hash-ref freq (string-ref s left) 0) 1))
      (set! left (+ left 1)))

    (set! max-length (max max-length (- (+ right 1 left 1)))))

  max-length)
```

**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Erlang

```erlang
-spec character_replacement(S :: unicode:unicode_binary(), K :: integer()) -> integer().
character_replacement(S, K) ->
    InputLength = size(S),
    character_replacement(S, K, 0, 0, 0, 0, #{}, InputLength).

character_replacement(_, _, MaxLength, _, _, Left, _, Left) ->
    MaxLength;
character_replacement(S, K, MaxLength, MaxFreq, Right, Left, Freq, InputLength) ->
    CurrentChar = binary:at(S, Right),
    FreqUpdated = maps:update_with(CurrentChar, fun(V) -> V + 1 end, 1, Freq),
    MaxFreqNew = max(maps:get(CurrentChar, FreqUpdated), MaxFreq),
    WindowSize = (Right - Left + 1),
    MaxLengthAdjusted = max(MaxLength, (Right - Left + 1)),
    {LeftNew, FreqFinal} =
        if
            (Right - Left + 1) - MaxFreqNew > K -> 
                {Left + 1, maps:update_with(binary:at(S, Left), fun(V) -> V - 1 end, FreqUpdated)};
            true -> {Left, FreqUpdated}
        end,
    character_replacement(S, K, MaxLengthAdjusted, MaxFreqNew, Right + 1, LeftNew, FreqFinal, InputLength).
```

**Time Complexity:** O(n)
**Space Complexity:** O(1)

### Elixir

```elixir
defmodule Solution do
  @spec character_replacement(s :: String.t, k :: integer) :: integer
  def character_replacement(s, k) do
    s_chars = String.graphemes(s)
    {max_length, _} =
      Enum.reduce(0..(length(s_chars) - 1), {0, %{left: 0, max_freq: 0, freq: %{}, s: s_chars, k: k}}, fn right, {max_length, acc} ->
        char = List.at(acc.s, right)
        freq = Map.update(acc.freq, char, 1, &(&1 + 1))
        max_freq = max(acc.max_freq, Map.get(freq, char))
        
        if (right - acc.left + 1) - max_freq > acc.k do
          freq = Map.update!(freq, List.at(acc.s, acc.left), &(&1 - 1))
          max_length = max(max_length, right - acc.left)
          %{acc | left: acc.left + 1, freq: freq, max_freq: max_freq}
        else
          %{acc | freq: freq, max_freq: max_freq}
        end
      end)
    
    max_length
  end
end
```

**Time Complexity:** O(n)
**Space Complexity:** O(1)


### Closing Statement

In our discussion, we explored the problem of finding the length of the longest substring with the same letter after performing at most `k` changes to a given string. We started by discussing a brute force approach, which, while straightforward, had an impractical time complexity of O(n^3). We then optimized our solution using a sliding window technique, which brought the time complexity down to a much more manageable O(n). We also discussed the space complexity, which remained at O(1) due to the fixed size of the character set.

Our conversation included coding this solution across multiple programming languages, demonstrating both the versatility of the sliding window approach and its consistent efficiency across different implementations.

### Similar Questions

To further practice and solidify the concepts and techniques discussed, you may want to try solving the following similar problems:

1. **Longest Substring Without Repeating Characters**
   - Given a string, find the length of the longest substring without repeating characters.

2. **Minimum Window Substring**
   - Given two strings `s` and `t`, return the minimum window substring of `s` such that every character in `t` (including duplicates) is included in the window. If there is no such substring, return the empty string.

3. **Fruit Into Baskets**
   - You are given an array of integers representing the types of fruits. You need to pick two types of fruits such that you have the longest subarray containing at most two different types of fruits.

4. **Longest Repeating Character Replacement (Generalized)**
   - Given a string and an integer `k`, return the length of the longest substring containing at most `k` distinct characters with the ability to replace characters.

5. **Length of Longest Subarray with At Most K Zeroes**
   - Given a binary array, find the length of the longest subarray that contains at most `k` zeroes.

By tackling these related problems, you can further develop your mastery of sliding window techniques and other efficient algorithms for string and array manipulation tasks. Good luck!