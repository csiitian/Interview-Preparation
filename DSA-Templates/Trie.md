# Trie Class

The `Trie` class is a tree-like data structure used for efficiently storing and searching strings. It is commonly used in tasks like auto-completion and spell checking.

## Components:

- `private TrieNode root;`: The root node of the Trie.
  
## Constructor:

### `Trie()`

Initializes an empty Trie with a root node.

## TrieNode Class:

The `TrieNode` class represents a single node in the Trie.

### Components:

- `private TrieNode[] children;`: Array of TrieNode references representing the children of the current node.
- `private boolean isEndOfWord;`: Flag indicating whether the node represents the end of a word.

### Constructor:

#### `TrieNode()`

Initializes a TrieNode with an array of size 26 (assuming only lowercase English letters) and sets `isEndOfWord` to `false`.

## Methods:

### `insert(String word)`

Inserts a word into the Trie. It traverses the Trie, creating nodes as necessary for each character in the word, and marks the last node as the end of the word.

### `search(String word)`

Searches for a word in the Trie. It traverses the Trie, checking if each character exists as a child node of the current node. Returns `true` if the word is found and ends in a node marked as the end of a word.

### `startsWith(String prefix)`

Checks if there is any word in the Trie that starts with the given prefix. It traverses the Trie, checking if each character exists as a child node of the current node. Returns `true` if a word is found that starts with the given prefix.

## Full Code:

```java []
class Trie {
    
    class TrieNode {
        TrieNode[] children;
        boolean isEndOfWord;
        
        TrieNode() {
            children = new TrieNode[26];
            isEndOfWord = false;
        }
    }
    
    private TrieNode root;
    
    Trie() {
        root = new TrieNode();
    }
    
    public void insert(String word) {
        TrieNode curr = root;
        for (char c : word.toCharArray()) {
            int index = c - 'a';
            if (curr.children[index] == null) {
                curr.children[index] = new TrieNode();
            }
            curr = curr.children[index];
        }
        curr.isEndOfWord = true;
    }
    
    public boolean search(String word) {
        TrieNode curr = root;
        for (char c : word.toCharArray()) {
            int index = c - 'a';
            if (curr.children[index] == null) {
                return false;
            }
            curr = curr.children[index];
        }
        return curr.isEndOfWord;
    }
    
    public boolean startsWith(String prefix) {
        TrieNode curr = root;
        for (char c : prefix.toCharArray()) {
            int index = c - 'a';
            if (curr.children[index] == null) {
                return false;
            }
            curr = curr.children[index];
        }
        return true;
    }

    public String findShortedPrefix(String word) {
        TrieNode curr = root;
        for(int i=0;i<word.length();i++) {
            int index = word.charAt(i) - 'a';
            if(curr.children[index] == null) {
                return "";
            }
            curr = curr.children[index];
            if(curr.isEndOfWord) {
                return word.substring(0, i+1);
            }
        }
        return "";
    }
}
```