### Interviewer and Interviewee Discussion

**Interviewer:**
The problem requires us to find all words from a given list that can be formed by sequentially adjacent letters on an `m x n` board. Each word must be constructed from letters of sequentially adjacent cells, and the same cell cannot be used more than once for a word. Here is an example:

**Example 1:**
```
Input: board = [
  ["o","a","a","n"],
  ["e","t","a","e"],
  ["i","h","k","r"],
  ["i","f","l","v"]
], words = ["oath","pea","eat","rain"]
Output: ["eat","oath"]
```

**Interviewee:**
Got it. This is a type of word search problem but now extended with a list of words. For each word, we need to track its formation through adjacent cells on the board.

**Interviewer:**
Exactly! Now, let's discuss how you would initially approach this problem. 

### Initial Thoughts Using Brute Force

**Interviewee:**
A brute force approach might involve searching each word individually in the board. Here are the steps I would take:

1. For each word in the list:
   1. For each cell in the board:
      1. Start a Depth-First Search (DFS) from the current cell to check if the word can be formed.
      2. Mark the cell as visited to avoid using it more than once.
      3. If the word is found, add it to the result list.
 
**Interviewer:**
Your approach is clear. Can you discuss the time and space complexity of this brute force solution?

**Interviewee:**
Sure. 

1. **Time Complexity:**
   - Let `W` be the length of the words list.
   - Let `L` be the average length of the words.
   - For each word `w` in words (W words), we perform a DFS search.
   - In the worst-case scenario, we need to search from every cell in the `m x n` board, resulting in `m * n` starting points.
   - DFS search itself will take `O(L)` as it needs to explore all characters of the word.
   - So, the total worst-case time complexity will be `O(W * m * n * L)`.

2. **Space Complexity:**
   - The space complexity mainly involves the storage of the path during DFS which will be `O(L)` at maximum depth.
   - Additionally, we need space for the visited array, which will be `O(m * n)`.
   - Hence, the overall space complexity is `O(m * n + L)`.

### Optimizing the Approach

**Interviewer:**
The brute force approach is a good start but given the constraints, it may not be efficient. Can you think of a way to optimize this search?

**Interviewee:**
Yes, we can use a Trie (prefix tree) to optimize the search for words. This will allow us to reduce redundant searches and backtracking while performing DFS. 

1. **Trie Construction:**
   - Insert all words into a Trie.

2. **Search with Trie:**
   - For each cell in the board, perform a DFS.
   - During the DFS, use the Trie to quickly check if a prefix of any word exists as we progress.
   - If we found a word, add it to our result list and mark it as found to avoid duplicate searches.

Using a Trie helps us prune the search space significantly – we stop the DFS path if a prefix doesn't exist in the Trie. 

### Drawing to Explain

Let's consider a partial visualization to demonstrate the concept:

Imagine the board:
```
o a a n
e t a e
i h k r
i f l v
```

And a Trie structure with words `oath`, `pea`, `eat`, `rain` inserted:
```
         root
        /  |  \
       o   p   e
      /    |   |
     a     e   a
    /      |   |
   t     a t --> (isWord)
  / n    /  |
 h__   n    |
  (isWord)  t
             ^
            (isWord)
```

Starting from cell \( (0, 0) \):
- 'o' -> 'a' -> 't' -> 'h' (Triggers finding "oath")
- Moving from cell \( (1, 1) \) -> 't' -> 'a' -> 'e' (Triggers finding "eat")

Both `oath` and `eat` are found without redundant searches, effectively leveraging the Trie.

### Conclusion

This optimized approach using a Trie combined with DFS will significantly reduce unnecessary backtracking and improve efficiency:
- **Time Complexity:** Reduced due to pruning of searches.
- **Space Complexity:** Trie will generally require additional space.

Would you like to proceed with the implementation of the optimized approach?
Sure, let's start by implementing the optimized solution using Trie (prefix tree) and Depth-First Search (DFS) in each of the provided languages. For each instance, I'll wrap the code in the provided method signatures.

### C++
```cpp
class Solution {
public:
    struct TrieNode {
        TrieNode* children[26] = {};
        string word = "";
    };
    
    void addWord(TrieNode* root, const string& word) {
        TrieNode* node = root;
        for (char c : word) {
            if (!node->children[c - 'a']) node->children[c - 'a'] = new TrieNode();
            node = node->children[c - 'a'];
        }
        node->word = word;
    }
    
    void dfs(vector<vector<char>>& board, int i, int j, TrieNode* node, vector<string>& result) {
        char c = board[i][j];
        if (c == '#' || !node->children[c - 'a']) return;
        node = node->children[c - 'a'];
        if (!node->word.empty()) {
            result.push_back(node->word);
            node->word = ""; // avoid duplicates
        }
        board[i][j] = '#'; // mark as visited
        if (i > 0) dfs(board, i-1, j, node, result);
        if (j > 0) dfs(board, i, j-1, node, result);
        if (i < board.size() - 1) dfs(board, i+1, j, node, result);
        if (j < board[0].size() - 1) dfs(board, i, j+1, node, result);
        board[i][j] = c; // unmark
    }
    
    vector<string> findWords(vector<vector<char>>& board, vector<string>& words) {
        vector<string> result;
        TrieNode* root = new TrieNode();
        for (const string& word : words) addWord(root, word);
        for (int i = 0; i < board.size(); ++i) {
            for (int j = 0; j < board[0].size(); ++j) {
                dfs(board, i, j, root, result);
            }
        }
        return result;
    }
};
```
### Time Complexity: O(m * n * 4^L) where L is the maximum length of the word.
### Space Complexity: O(WL + m * n) where W is the number of words and L is the length of each word.
___

### Java
```java
class Solution {
    class TrieNode {
        TrieNode[] children = new TrieNode[26];
        String word = null;
    }

    public void addWord(TrieNode root, String word) {
        TrieNode node = root;
        for (char c : word.toCharArray()) {
            if (node.children[c - 'a'] == null) node.children[c - 'a'] = new TrieNode();
            node = node.children[c - 'a'];
        }
        node.word = word;
    }

    public void dfs(char[][] board, int i, int j, TrieNode node, List<String> result) {
        char c = board[i][j];
        if (c == '#' || node.children[c - 'a'] == null) return;
        node = node.children[c - 'a'];
        if (node.word != null) {
            result.add(node.word);
            node.word = null; // avoid duplicates
        }
        board[i][j] = '#'; // mark as visited
        if (i > 0) dfs(board, i-1, j, node, result);
        if (j > 0) dfs(board, i, j-1, node, result);
        if (i < board.length - 1) dfs(board, i+1, j, node, result);
        if (j < board[0].length - 1) dfs(board, i, j+1, node, result);
        board[i][j] = c; // unmark
    }

    public List<String> findWords(char[][] board, String[] words) {
        List<String> result = new ArrayList<>();
        TrieNode root = new TrieNode();
        for (String word : words) addWord(root, word);
        for (int i = 0; i < board.length; ++i) {
            for (int j = 0; j < board[0].length; ++j) {
                dfs(board, i, j, root, result);
            }
        }
        return result;
    }
}
```
### Time Complexity: O(m * n * 4^L) where L is the maximum length of the word.
### Space Complexity: O(WL + m * n) where W is the number of words and L is the length of each word.
___

### Python
```python
class Solution(object):
    def findWords(self, board, words):
        """
        :type board: List[List[str]]
        :type words: List[str]
        :rtype: List[str]
        """
        class TrieNode:
            def __init__(self):
                self.children = {}
                self.word = None

        def addWord(word):
            node = root
            for letter in word:
                if letter not in node.children:
                    node.children[letter] = TrieNode()
                node = node.children[letter]
            node.word = word

        def dfs(x, y, node):
            letter = board[x][y]
            currNode = node.children.get(letter)
            if not currNode:
                return
            if currNode.word:
                foundWords.append(currNode.word)
                currNode.word = None  # avoid duplicate

            board[x][y] = '#'
            for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                nx, ny = x + dx, y + dy
                if 0 <= nx < len(board) and 0 <= ny < len(board[0]):
                    dfs(nx, ny, currNode)
            board[x][y] = letter

        root = TrieNode()
        for word in words:
            addWord(word)
        
        foundWords = []
        for i in range(len(board)):
            for j in range(len(board[0])):
                dfs(i, j, root)
                
        return foundWords
```
### Time Complexity: O(m * n * 4^L) where L is the maximum length of the word.
### Space Complexity: O(WL + m * n) where W is the number of words and L is the length of each word.
___

### Python3
```python
class Solution:
    def findWords(self, board: List[List[str]], words: List[str]) -> List[str]:
        class TrieNode:
            def __init__(self):
                self.children = {}
                self.word = None

        def addWord(word):
            node = root
            for letter in word:
                if letter not in node.children:
                    node.children[letter] = TrieNode()
                node = node.children[letter]
            node.word = word

        def dfs(x, y, node):
            letter = board[x][y]
            currNode = node.children.get(letter)
            if not currNode:
                return
            if currNode.word:
                foundWords.append(currNode.word)
                currNode.word = None  # avoid duplicate

            board[x][y] = '#'
            for dx, dy in [(-1, 0), (1, 0), (0, -1), (0, 1)]:
                nx, ny = x + dx, y + dy
                if 0 <= nx < len(board) and 0 <= ny < len(board[0]):
                    dfs(nx, ny, currNode)
            board[x][y] = letter

        root = TrieNode()
        for word in words:
            addWord(word)
        
        foundWords = []
        for i in range(len(board)):
            for j in range(len(board[0])):
                dfs(i, j, root)
                
        return foundWords
```
### Time Complexity: O(m * n * 4^L) where L is the maximum length of the word.
### Space Complexity: O(WL + m * n) where W is the number of words and L is the length of each word.
___

### C
```c
/**
 * Note: The returned array must be malloced, assume caller calls free().
 */
typedef struct TrieNode {
    struct TrieNode* children[26];
    char* word;
} TrieNode;

TrieNode* createNode() {
    TrieNode* node = (TrieNode*)malloc(sizeof(TrieNode));
    int i;
    for (i = 0; i < 26; i++) node->children[i] = NULL;
    node->word = NULL;
    return node;
}

void addWord(TrieNode* root, char* word) {
    TrieNode* node = root;
    while (*word) {
        if (!node->children[*word - 'a']) node->children[*word - 'a'] = createNode();
        node = node->children[*word - 'a'];
        word++;
    }
    node->word = word;
}

void dfs(char** board, int boardSize, int* boardColSize,
         int i, int j, TrieNode* node, char** result, int* returnSize) {
    char c = board[i][j];
    if (c == '#' || !node->children[c - 'a']) return;
    node = node->children[c - 'a'];
    if (node->word) {
        result[(*returnSize)++] = node->word;
        node->word = NULL;
    }
    board[i][j] = '#';
    if (i > 0) dfs(board, boardSize, boardColSize, i-1, j, node, result, returnSize);
    if (j > 0) dfs(board, boardSize, boardColSize, i, j-1, node, result, returnSize);
    if (i < boardSize - 1) dfs(board, boardSize, boardColSize, i+1, j, node, result, returnSize);
    if (j < boardColSize[i] - 1) dfs(board, boardSize, boardColSize, i, j+1, node, result, returnSize);
    board[i][j] = c;
}

char** findWords(char** board, int boardSize, int* boardColSize, char** words, int wordsSize, int* returnSize) {
    TrieNode* root = createNode();
    for (int i = 0; i < wordsSize; i++) addWord(root, words[i]);
    
    char** result = (char**)malloc(wordsSize * sizeof(char*));
    *returnSize = 0;
    for (int i = 0; i < boardSize; i++) {
        for (int j = 0; j < boardColSize[i]; j++) {
            dfs(board, boardSize, boardColSize, i, j, root, result, returnSize);
        }
    }
    
    return result;
}
```
### Time Complexity: O(m * n * 4^L) where L is the maximum length of the word.
### Space Complexity: O(WL + m * n) where W is the number of words and L is the length of each word.
___

### C#
```csharp
public class Solution {
    class TrieNode {
        public TrieNode[] children = new TrieNode[26];
        public string word = null;
    }

    public void AddWord(TrieNode root, string word) {
        TrieNode node = root;
        foreach (char c in word) {
            if (node.children[c - 'a'] == null) node.children[c - 'a'] = new TrieNode();
            node = node.children[c - 'a'];
        }
        node.word = word;
    }

    public void DFS(char[][] board, int i, int j, TrieNode node, IList<string> result) {
        char c = board[i][j];
        if (c == '#' || node.children[c - 'a'] == null) return;
        node = node.children[c - 'a'];
        if (node.word != null) {
            result.Add(node.word);
            node.word = null;
        }
        board[i][j] = '#';
        if (i > 0) DFS(board, i-1, j, node, result);
        if (j > 0) DFS(board, i, j-1, node, result);
        if (i < board.Length - 1) DFS(board, i+1, j, node, result);
        if (j < board[0].Length - 1) DFS(board, i, j+1, node, result);
        board[i][j] = c;
    }

    public IList<string> FindWords(char[][] board, string[] words) {
        List<string> result = new List<string>();
        TrieNode root = new TrieNode();
        foreach (string word in words) AddWord(root, word);
        
        for (int i = 0; i < board.Length; ++i) {
            for (int j = 0; j < board[i].Length; ++j) {
                DFS(board, i, j, root, result);
            }
        }
        return result;
    }
}
```
### Time Complexity: O(m * n * 4^L) where L is the maximum length of the word.
### Space Complexity: O(WL + m * n) where W is the number of words and L is the length of each word.
___

### JavaScript
```javascript
/**
 * @param {character[][]} board
 * @param {string[]} words
 * @return {string[]}
 */
var findWords = function(board, words) {
    class TrieNode {
        constructor() {
            this.children = {};
            this.word = null;
        }
    }
    
    const addWord = (word) => {
        let node = root;
        for (let c of word) {
            if (!node.children[c]) {
                node.children[c] = new TrieNode();
            }
            node = node.children[c];
        }
        node.word = word;
    };
    
    const dfs = (x, y, node) => {
        const c = board[x][y];
        if (!node.children[c]) return;
        node = node.children[c];
        if (node.word) {
            foundWords.push(node.word);
            node.word = null;
        }
        board[x][y] = '#';  // mark as visited
        const directions = [[-1, 0], [1, 0], [0, -1], [0, 1]];
        for (let [dx, dy] of directions) {
            const nx = x + dx, ny = y + dy;
            if (nx >= 0 && ny >= 0 && nx < board.length && ny < board[0].length) {
                dfs(nx, ny, node);
            }
        }
        board[x][y] = c;  // restore
    };
    
    let root = new TrieNode();
    for (let word of words) addWord(word);
    
    let foundWords = [];
    for (let i = 0; i < board.length; ++i) {
        for (let j = 0; j < board[i].length; ++j) {
            dfs(i, j, root);
        }
    }
    
    return foundWords;
};
```
### Time Complexity: O(m * n * 4^L) where L is the maximum length of the word.
### Space Complexity: O(WL + m * n) where W is the number of words and L is the length of each word.
___

### TypeScript
```typescript
function findWords(board: string[][], words: string[]): string[] {
    class TrieNode {
        children: Map<string, TrieNode> = new Map();
        word: string | null = null;
    }

    function addWord(word: string) {
        let node = root;
        for (let c of word) {
            if (!node.children.has(c)) {
                node.children.set(c, new TrieNode());
            }
            node = node.children.get(c)!;
        }
        node.word = word;
    }

    function dfs(x: number, y: number, node: TrieNode) {
        const c = board[x][y];
        if (!node.children.has(c)) return;
        node = node.children.get(c)!;
        if (node.word) {
            foundWords.push(node.word);
            node.word = null;
        }
        board[x][y] = '#';  // mark as visited
        const directions = [[-1, 0], [1, 0], [0, -1], [0, 1]];
        for (let [dx, dy] of directions) {
            const nx = x + dx, ny = y + dy;
            if (nx >= 0 && ny >= 0 && nx < board.length && ny < board[0].length) {
                dfs(nx, ny, node);
            }
        }
        board[x][y] = c;  // restore
    }

    let root = new TrieNode();
    for (let word of words) addWord(word);

    let foundWords: string[] = [];
    for (let i = 0; i < board.length; ++i) {
        for (let j = 0; j < board[i].length; ++j) {
            dfs(i, j, root);
        }
    }

    return foundWords;
};
```
### Time Complexity: O(m * n * 4^L) where L is the maximum length of the word.
### Space Complexity: O(WL + m * n) where W is the number of words and L is the length of each word.
___


### Closing Statement

We've discussed a comprehensive approach to solving the word search problem on a 2D board of characters using Tries and Depth-First Search (DFS). We started with a brute-force approach, analyzed its complexities, then optimized the solution using a Trie, which significantly reduces the search space, making it more efficient.

The implementations provided cater to various programming languages and include the necessary operations to build the Trie and perform the DFS search, marking cells as visited and unvisited efficiently. The solution ensures that no cell is used more than once in forming a word and effectively handles backtracking.

This problem is an excellent way to understand the application of Tries in conjunction with search algorithms to solve complex problems efficiently.

### Similar Questions

Here are some similar problems you might find interesting:

1. **Word Search** (LeetCode #79)
   - Given a board and a word, check if the word exists in the board.
   - [View Problem](https://leetcode.com/problems/word-search/)

2. **Word Search II** (LeetCode #212)
   - Find all words in a given list from a 2D board of characters.
   - [View Problem](https://leetcode.com/problems/word-search-ii/)

3. **Longest Word in Dictionary** (LeetCode #720)
   - Given a list of words, return the longest word in the dictionary that can be built one character at a time by other words in the dictionary.
   - [View Problem](https://leetcode.com/problems/longest-word-in-dictionary/)

4. **Add and Search Word - Data Structure Design** (LeetCode #211)
   - Design a data structure that supports adding new words and finding if a string matches any previously added string.
   - [View Problem](https://leetcode.com/problems/add-and-search-word-data-structure-design/)

5. **Implement Trie (Prefix Tree)** (LeetCode #208)
   - Implement a trie with insert, search, and startsWith methods.
   - [View Problem](https://leetcode.com/problems/implement-trie-prefix-tree/)

6. **Concatenated Words** (LeetCode #472)
   - Given a list of words (without duplicates), find all the concatenated words in the list.
   - [View Problem](https://leetcode.com/problems/concatenated-words/)

7. **Autocomplete System** (LeetCode #642)
   - Design a search autocomplete system for a given set of sentences and corresponding times.
   - [View Problem](https://leetcode.com/problems/design-search-autocomplete-system/)

By practicing these similar problems, you can deepen your understanding of Trie data structures and their applications in search-related problems.