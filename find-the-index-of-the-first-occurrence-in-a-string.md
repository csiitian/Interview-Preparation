### Interviewer and Interviewee Discussion

**Interviewer:** Let's discuss a problem where you need to find a substring within a string. Given two strings `needle` and `haystack`, you have to return the index of the first occurrence of `needle` in `haystack`, or `-1` if `needle` is not part of `haystack`.

**Interviewee:** Sure, I understand the problem. To return the index of the first occurrence of `needle` in `haystack`, I will search for `needle` in `haystack`. If I find it, I'll return the starting index of the `needle`, otherwise, I'll return -1.

**Interviewer:** Sounds good. Let's talk about a brute force approach first. How would you implement that?

**Interviewee:** For a brute force approach, I’ll iterate over each possible starting point in `haystack` where `needle` could fit. For each starting point, I’ll check if the substring starting at that point matches `needle`. If it does, I’ll return that starting index. If I go through the entire `haystack` without finding `needle`, I’ll return -1.

### Brute Force Approach

#### Pseudocode:
```python
def strStr(haystack, needle):
    n, m = len(haystack), len(needle)
    
    for i in range(n - m + 1):
        if haystack[i:i + m] == needle:
            return i
    return -1
```

#### Time Complexity:
- **Outer loop:** Runs from 0 to `n - m` (where `n` is the length of `haystack` and `m` is the length of `needle`), so it runs `O(n - m)` times in the worst case.
- **Substring comparison:** Each comparison takes `O(m)` time.
- Overall time complexity is `O((n - m) * m)`, which simplifies to `O(n * m)` in the worst case.

#### Space Complexity:
- Space complexity is `O(1)` since we are not using any extra space proportional to the input size.

**Interviewer:** That makes sense. The brute force method is straightforward but can be improved. How would you optimize this approach?

**Interviewee:** We can employ string searching algorithms like the Knuth-Morris-Pratt (KMP) algorithm, which preprocesses the `needle` to determine the longest prefix which is also a suffix (LPS array). This allows us to skip characters while matching, leading to a linear time complexity.

### Optimized Approach: Knuth-Morris-Pratt (KMP) Algorithm

#### Steps:
1. **Build the LPS array for `needle`.**
2. **Use the LPS array to search the `needle` in `haystack`.**

#### Pseudocode:
```python
def computeLPS(needle):
    m = len(needle)
    lps = [0] * m
    j = 0  # length of the previous longest prefix suffix
    
    i = 1
    while i < m:
        if needle[i] == needle[j]:
            j += 1
            lps[i] = j
            i += 1
        else:
            if j != 0:
                j = lps[j - 1]
            else:
                lps[i] = 0
                i += 1
    return lps

def strStr(haystack, needle):
    n, m = len(haystack), len(needle)
    if m == 0:
        return 0
    
    lps = computeLPS(needle)
    i = j = 0
    
    while i < n:
        if haystack[i] == needle[j]:
            i += 1
            j += 1
        
        if j == m:
            return i - j
        elif i < n and haystack[i] != needle[j]:
            if j != 0:
                j = lps[j - 1]
            else:
                i += 1
    return -1
```

#### Time Complexity:
- **Building LPS array:** Takes `O(m)` time.
- **String Search:** Takes `O(n)` time due to potential skips.
- Overall time complexity is `O(n + m)`.

#### Space Complexity:
- Space complexity is `O(m)` due to the LPS array.

### Visualization:

Consider `haystack = "ababcabcabababd"` and `needle = "ababd"`.

1. **LPS Array Construction for `needle`:**

```
needle:  a b a b d
LPS:     0 0 1 2 0
```

2. **Matching Process:**

```
haystack: a b a b c a b c a b a b a b d
needle:   a b a b d
                    ↑ Successful match found.
```

The KMP algorithm significantly reduces redundant comparisons by utilizing the LPS array, making the search process more efficient.
Sure, here's the implementation of the KMP algorithm in the various languages as specified:

### C++
```cpp
class Solution {
public:
    int strStr(string haystack, string needle) {
        int n = haystack.size(), m = needle.size();
        if (m == 0) return 0;
        
        vector<int> lps = computeLPS(needle);
        int i = 0, j = 0;
        while (i < n) {
            if (haystack[i] == needle[j]) {
                i++;
                j++;
            }
            if (j == m) {
                return i - j;
            } else if (i < n && haystack[i] != needle[j]) {
                if (j != 0) {
                    j = lps[j - 1];
                } else {
                    i++;
                }
            }
        }
        return -1;
    }
    
private:
    vector<int> computeLPS(string needle) {
        int m = needle.size();
        vector<int> lps(m, 0);
        int j = 0;
        for (int i = 1; i < m;) {
            if (needle[i] == needle[j]) {
                lps[i] = j + 1;
                i++;
                j++;
            } else {
                if (j != 0) {
                    j = lps[j - 1];
                } else {
                    lps[i] = 0;
                    i++;
                }
            }
        }
        return lps;
    }
};
```

### Java
```java
class Solution {
    public int strStr(String haystack, String needle) {
        int n = haystack.length(), m = needle.length();
        if (m == 0) return 0;

        int[] lps = computeLPS(needle);
        int i = 0, j = 0;
        while (i < n) {
            if (haystack.charAt(i) == needle.charAt(j)) {
                i++;
                j++;
            }
            if (j == m) {
                return i - j;
            } else if (i < n && haystack.charAt(i) != needle.charAt(j)) {
                if (j != 0) {
                    j = lps[j - 1];
                } else {
                    i++;
                }
            }
        }
        return -1;
    }

    private int[] computeLPS(String needle) {
        int m = needle.length();
        int[] lps = new int[m];
        int j = 0;
        
        for (int i = 1; i < m;) {
            if (needle.charAt(i) == needle.charAt(j)) {
                lps[i] = j + 1;
                i++;
                j++;
            } else {
                if (j != 0) {
                    j = lps[j - 1];
                } else {
                    lps[i] = 0;
                    i++;
                }
            }
        }
        return lps;
    }
}
```

### Python
```python
class Solution(object):
    def strStr(self, haystack, needle):
        """
        :type haystack: str
        :type needle: str
        :rtype: int
        """
        n, m = len(haystack), len(needle)
        if m == 0:
            return 0
        
        lps = self.computeLPS(needle)
        i = j = 0
        
        while i < n:
            if haystack[i] == needle[j]:
                i += 1
                j += 1
            
            if j == m:
                return i - j
            elif i < n and haystack[i] != needle[j]:
                if j != 0:
                    j = lps[j - 1]
                else:
                    i += 1
        return -1
    
    def computeLPS(self, needle):
        m = len(needle)
        lps = [0] * m
        j = 0
        
        for i in range(1, m):
            if needle[i] == needle[j]:
                j += 1
                lps[i] = j
            else:
                if j != 0:
                    j = lps[j - 1]
                    i -= 1  # stay at the same `i` in next iteration
                else:
                    lps[i] = 0
        return lps
```

### Python3
```python
class Solution:
    def strStr(self, haystack: str, needle: str) -> int:
        n, m = len(haystack), len(needle)
        if m == 0:
            return 0
        
        lps = self.computeLPS(needle)
        i = j = 0
        
        while i < n:
            if haystack[i] == needle[j]:
                i += 1
                j += 1
            
            if j == m:
                return i - j
            elif i < n and haystack[i] != needle[j]:
                if j != 0:
                    j = lps[j - 1]
                else:
                    i += 1
        return -1
    
    def computeLPS(self, needle: str) -> list[int]:
        m = len(needle)
        lps = [0] * m
        j = 0
        
        for i in range(1, m):
            if needle[i] == needle[j]:
                j += 1
                lps[i] = j
            else:
                if j != 0:
                    j = lps[j - 1]
                    i -= 1  # stay at the same `i` in next iteration
                else:
                    lps[i] = 0
        return lps
```

### C
```c
#include <string.h>

int* computeLPS(char* needle, int m) {
    int* lps = (int*)malloc(m * sizeof(int));
    int j = 0;
    lps[0] = 0;
    
    for (int i = 1; i < m;) {
        if (needle[i] == needle[j]) {
            j++;
            lps[i] = j;
            i++;
        } else {
            if (j != 0) {
                j = lps[j - 1];
            } else {
                lps[i] = 0;
                i++;
            }
        }
    }
    return lps;
}

int strStr(char* haystack, char* needle) {
    int n = strlen(haystack);
    int m = strlen(needle);
    if (m == 0) return 0;

    int* lps = computeLPS(needle, m);
    int i = 0, j = 0;

    while (i < n) {
        if (haystack[i] == needle[j]) {
            i++;
            j++;
        }
        if (j == m) {
            free(lps);
            return i - j;
        } else if (i < n && haystack[i] != needle[j]) {
            if (j != 0) {
                j = lps[j - 1];
            } else {
                i++;
            }
        }
    }
    free(lps);
    return -1;
}
```

### C#
```csharp
public class Solution {
    public int StrStr(string haystack, string needle) {
        int n = haystack.Length, m = needle.Length;
        if (m == 0) return 0;

        int[] lps = ComputeLPS(needle);
        int i = 0, j = 0;
        while (i < n) {
            if (haystack[i] == needle[j]) {
                i++;
                j++;
            }
            if (j == m) {
                return i - j;
            } else if (i < n && haystack[i] != needle[j]) {
                if (j != 0) {
                    j = lps[j - 1];
                } else {
                    i++;
                }
            }
        }
        return -1;
    }

    private int[] ComputeLPS(string needle) {
        int m = needle.Length;
        int[] lps = new int[m];
        int j = 0;

        for (int i = 1; i < m;) {
            if (needle[i] == needle[j]) {
                lps[i] = j + 1;
                i++;
                j++;
            } else {
                if (j != 0) {
                    j = lps[j - 1];
                } else {
                    lps[i] = 0;
                    i++;
                }
            }
        }
        return lps;
    }
}
```

### JavaScript
```javascript
/** 
 * @param {string} haystack 
 * @param {string} needle 
 * @return {number} 
 */
var strStr = function(haystack, needle) {
    var n = haystack.length, m = needle.length;
    if (m === 0) return 0;

    var lps = computeLPS(needle);
    var i = 0, j = 0;

    while (i < n) {
        if (haystack[i] === needle[j]) {
            i++;
            j++;
        }
        
        if (j === m) {
            return i - j;
        } else if (i < n && haystack[i] !== needle[j]) {
            if (j !== 0) {
                j = lps[j - 1];
            } else {
                i++;
            }
        }
    }
    return -1;
};

/**
 * @param {string} needle 
 * @return {number[]} 
 */
function computeLPS(needle) {
    var m = needle.length;
    var lps = new Array(m).fill(0);
    var j = 0;

    for (var i = 1; i < m;) {
        if (needle[i] === needle[j]) {
            lps[i] = j + 1;
            i++;
            j++;
        } else {
            if (j !== 0) {
                j = lps[j - 1];
            } else {
                lps[i] = 0;
                i++;
            }
        }
    }
    return lps;
}
```

### TypeScript
```typescript
function strStr(haystack: string, needle: string): number {
    let n = haystack.length, m = needle.length;
    if (m === 0) return 0;

    let lps = computeLPS(needle);
    let i = 0, j = 0;

    while (i < n) {
        if (haystack[i] === needle[j]) {
            i++;
            j++;
        }
        if (j === m) {
            return i - j;
        } else if (i < n && haystack[i] !== needle[j]) {
            if (j !== 0) {
                j = lps[j - 1];
            } else {
                i++;
            }
        }
    }
    return -1;
}

function computeLPS(needle: string): number[] {
    let m = needle.length;
    let lps = new Array(m).fill(0);
    let j = 0;

    for (let i = 1; i < m;) {
        if (needle[i] === needle[j]) {
            lps[i] = j + 1;
            i++;
            j++;
        } else {
            if (j !== 0) {
                j = lps[j - 1];
            } else {
                lps[i] = 0;
                i++;
            }
        }
    }
    return lps;
}
```

### PHP
```php
class Solution {

    /**
     * @param String $haystack
     * @param String $needle
     * @return Integer
     */
    function strStr($haystack, $needle) {
        $n = strlen($haystack);
        $m = strlen($needle);
        if ($m == 0) return 0;

        $lps = $this->computeLPS($needle);
        $i = $j = 0;

        while ($i < $n) {
            if ($haystack[$i] == $needle[$j]) {
                $i++;
                $j++;
            }
            if ($j == $m) {
                return $i - $j;
            } else if ($i < $n && $haystack[$i] != $needle[$j]) {
                if ($j != 0) {
                    $j = $lps[$j - 1];
                } else {
                    $i++;
                }
            }
        }
        return -1;
    }

    function computeLPS($needle) {
        $m = strlen($needle);
        $lps = array_fill(0, $m, 0);
        $j = 0;

        for ($i = 1; $i < $m;) {
            if ($needle[$i] == $needle[$j]) {
                $j++;
                $lps[$i] = $j;
                $i++;
            } else {
                if ($j != 0) {
                    $j = $lps[$j - 1];
                } else {
                    $lps[$i] = 0;
                    $i++;
                }
            }
        }
        return $lps;
    }
}
```

### Swift
```swift
class Solution {
    func strStr(_ haystack: String, _ needle: String) -> Int {
        let n = haystack.count
        let m = needle.count
        if m == 0 { return 0 }

        let lps = computeLPS(needle)
        var i = 0, j = 0

        let haystackArray = Array(haystack)
        let needleArray = Array(needle)

        while i < n {
            if haystackArray[i] == needleArray[j] {
                i += 1
                j += 1
            }
            if j == m {
                return i - j
            } else if i < n && haystackArray[i] != needleArray[j] {
                if j != 0 {
                    j = lps[j - 1]
                } else {
                    i += 1
                }
            }
        }
        return -1
    }

    func computeLPS(_ needle: String) -> [Int] {
        let m = needle.count
        var lps = Array(repeating: 0, count: m)
        var j = 0

        let needleArray = Array(needle)

        for i in 1..<m {
            if needleArray[i] == needleArray[j] {
                j += 1
                lps[i] = j
            } else {
                if j != 0 {
                    j = lps[j - 1]
                } else {
                    lps[i] = 0
                }
            }
        }
        return lps
    }
}
```

### Kotlin
```kotlin
class Solution {
    fun strStr(haystack: String, needle: String): Int {
        val n = haystack.length
        val m = needle.length
        if (m == 0) return 0

        val lps = computeLPS(needle)
        var i = 0
        var j = 0

        while (i < n) {
            if (haystack[i] == needle[j]) {
                i++
                j++
            }
            if (j == m) {
                return i - j
            } else if (i < n && haystack[i] != needle[j]) {
                if (j != 0) {
                    j = lps[j - 1]
                } else {
                    i++
                }
            }
        }
        return -1
    }

    private fun computeLPS(needle: String): IntArray {
        val m = needle.length
        val lps = IntArray(m)
        var j = 0

        for (i in 1 until m) {
            if (needle[i] == needle[j]) {
                j++
                lps[i] = j
            } else {
                if (j != 0) {
                    j = lps[j - 1]
                } else {
                    lps[i] = 0
                }
            }
        }
        return lps
    }
}
```

### Dart
```dart
class Solution {
  int strStr(String haystack, String needle) {
    int n = haystack.length, m = needle.length;
    if (m == 0) return 0;

    List<int> lps = computeLPS(needle);
    int i = 0, j = 0;

    while (i < n) {
      if (haystack[i] == needle[j]) {
        i++;
        j++;
      }
      if (j == m) {
        return i - j;
      } else if (i < n && haystack[i] != needle[j]) {
        if (j != 0) {
          j = lps[j - 1];
        } else {
          i++;
        }
      }
    }
    return -1;
  }

  List<int> computeLPS(String needle) {
    int m = needle.length;
    List<int> lps = List.filled(m, 0);
    int j = 0;

    for (int i = 1; i < m;) {
      if (needle[i] == needle[j]) {
        lps[i] = j + 1;
        i++;
        j++;
      } else {
        if (j != 0) {
          j = lps[j - 1];
        } else {
          lps[i] = 0;
          i++;
        }
      }
    }
    return lps;
  }
}
```

### Go
```go
func strStr(haystack string, needle string) int {
    n, m := len(haystack), len(needle)
    if m == 0 {
        return 0
    }

    lps := computeLPS(needle)
    i, j := 0, 0

    for i < n {
        if haystack[i] == needle[j] {
            i++
            j++
        }
        if j == m {
            return i - j
        } else if i < n && haystack[i] != needle[j] {
            if j != 0 {
                j = lps[j - 1]
            } else {
                i++
            }
        }
    }
    return -1
}

func computeLPS(needle string) []int {
    m := len(needle)
    lps := make([]int, m)
    j := 0

    for i := 1; i < m; {
        if needle[i] == needle[j] {
            j++
            lps[i] = j
            i++
        } else {
            if j != 0 {
                j = lps[j - 1]
            } else {
                lps[i] = 0
                i++
            }
        }
    }
    return lps
}
```

### Closing Statement

Thank you for the in-depth discussion on the substring search problem. We explored a brute force method and then optimized our approach using the Knuth-Morris-Pratt (KMP) algorithm to achieve a more efficient solution. The KMP algorithm effectively uses preprocessing to handle the substring search in linear time complexity, `O(n + m)`, making it a powerful tool for this type of problem.

Implementing and understanding the KMP algorithm in multiple programming languages not only solidifies your understanding of the algorithm but also prepares you for varied interviews across different technical stacks.

### Similar Questions

Here are some similar questions that will further help solidify your understanding of string manipulation and substring search:

1. **Implement strStr() – Variation**: Implement a function to find the last occurrence of `needle` in `haystack`.

2. **Pattern Matching with Wildcards**: Given a `text` and a `pattern`, implement wildcard pattern matching with support for `?` (matches any single character) and `*` (matches any sequence of characters including the empty sequence).

3. **Longest Prefix Suffix**: Write a function to find the length of the longest prefix which is also a suffix in a given string.

4. **Rabin-Karp Algorithm for Pattern Searching**: Implement the Rabin-Karp algorithm for searching a pattern in a given text.

5. **Anagram Substring Search**: Given a string `s` and a pattern `p`, return the start indices of `p`'s anagrams in `s`.

6. **Count Occurrences of Anagrams**: Given a text and a pattern, find the count of all anagrams of the pattern in the text.

7. **Z Algorithm for Pattern Matching**: Implement the Z algorithm for pattern matching which preprocesses the pattern to provide linear search time for matches.

8. **Smallest Window Containing All Characters of Another String**: Given two strings `s` and `t`, find the minimum window in `s` which will contain all the characters in `t`.

By solving these related problems, you can strengthen your understanding of string algorithms, which is a vital part of coding interviews and competitive programming.