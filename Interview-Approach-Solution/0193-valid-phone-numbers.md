### Interviewer and Interviewee Discussion:

**Interviewer:**
We're looking for a solution to extract valid phone numbers from a text file using a one-liner bash script. The phone numbers can appear in two valid formats: (xxx) xxx-xxxx or xxx-xxx-xxxx. How would you approach this problem?

**Interviewee:**
To start, I'd look at each line of the file and determine whether it matches one of the two specified formats. A simple way to achieve this is by using regular expressions, given their ability to match specific patterns within strings.

**Interviewer:**
That sounds like a good start. Could you describe a brute force approach for this problem?

**Interviewee:**
Sure. A brute force approach would be to read each line of the file, check if the line matches either of the regular expression patterns for valid phone numbers, and if so, print that line. In bash, we can accomplish this using tools like `grep`.

**Interviewer:**
Great! Let's say you used `grep` to implement this brute force approach. What would the time and space complexity of your solution be?

**Interviewee:**
Assuming `n` is the number of lines in the file:

- **Time Complexity:** O(n), since `grep` processes each line of the file.
- **Space Complexity:** O(1), as we are only checking each line against the pattern without storing additional data (besides potentially the lines themselves, depending on the implementation of `grep`).

**Interviewer:**
That sounds correct. Can you write the actual one-liner bash command to solve this?

**Interviewee:**
Of course. Here’s the one-liner bash script using `grep`:

```bash
grep -E '^\(\d{3}\) \d{3}-\d{4}$|^\d{3}-\d{3}-\d{4}$' file.txt
```

This command uses `grep` with the `-E` flag to enable extended regular expressions. The pattern `^\(\d{3}\) \d{3}-\d{4}$|^\d{3}-\d{3}-\d{4}$` captures both valid phone number formats.

**Interviewer:**
Nice work. Just to clarify, could you explain how the regular expressions work?

**Interviewee:**
Sure. The regular expression can be broken down into two parts separated by the `|` (alternation operator):
- `^\(\d{3}\) \d{3}-\d{4}$`: This matches the format `(xxx) xxx-xxxx`.
  - `^`: Asserts the position at the start of the line.
  - `\(\d{3}\)`: Matches exactly three digits enclosed in parentheses `()`.
  - ` `: Matches a single space.
  - `\d{3}`: Matches exactly three digits.
  - `-`: Matches a hyphen.
  - `\d{4}$`: Matches exactly four digits which should be at the end of the line.
- `^\d{3}-\d{3}-\d{4}$`: This matches the format `xxx-xxx-xxxx`.
  - `^`: Asserts the position at the start of the line.
  - `\d{3}`: Matches exactly three digits.
  - `-`: Matches a hyphen.
  - `\d{3}`: Matches exactly three digits.
  - `-`: Matches a hyphen.
  - `\d{4}$`: Matches exactly four digits which should be at the end of the line.

### Drawing Explanation

To further explain this visually:

1. (xxx) xxx-xxxx
   - `(123) 456-7890`

   |     | 1 | 2 | 3 |       | 4 | 5 | 6 |   | 7 | 8 | 9 | 0 |
   |-----|---|---|---|-------|---|---|---|---|---|---|---|
   | Char| ( | 1 | 2 | 3 | ) |   | 4 | 5 | 6 | - | 7 | 8 | 9 | 0 |
  
2. xxx-xxx-xxxx
   - `987-123-4567`

    | 9 | 8 | 7 | - | 1 | 2 | 3 | - | 4 | 5 | 6 | 7 |  
    |---|---|---|---|---|---|---|---|---|---|---|---|---

Each part of the format gets matched by the regular expression segments as explained previously.

**Interviewer:**
Thank you for the detailed explanation and the visual representation. That clears things up nicely.
Certainly! Here's how the implementation would look like in various languages along with their time and space complexity explained:

```bash
# Bash
# Read from the file file.txt and output all valid phone numbers to stdout.

read_valid_phone_numbers() {
  grep -E '^\(\d{3}\) \d{3}-\d{4}$|^\d{3}-\d{3}-\d{4}$' file.txt
}
# Explanation:
# Time Complexity: O(n) - where n is the number of lines in file.txt.
# Space Complexity: O(1) - as we do not use any extra data structures to store the lines.

read_valid_phone_numbers
```

```python
# Python
# Read from the file file.txt and output all valid phone numbers to stdout.
import re

def read_valid_phone_numbers():
    with open('file.txt', 'r') as file:
        for line in file:
            if re.match(r'^\(\d{3}\) \d{3}-\d{4}$|^\d{3}-\d{3}-\d{4}$', line.strip()):
                print(line.strip())

# Explanation:
# Time Complexity: O(n) - where n is the number of lines in file.txt.
# Space Complexity: O(1) - no additional data structures are used, apart from file input.

read_valid_phone_numbers()
```

```java
// Java
// Read from the file file.txt and output all valid phone numbers to stdout.

import java.io.*;
import java.util.regex.*;

public class PhoneNumbersValidator {
    public static void main(String[] args) {
        readValidPhoneNumbers();
    }

    public static void readValidPhoneNumbers() {
        try (BufferedReader br = new BufferedReader(new FileReader("file.txt"))) {
            Pattern pattern = Pattern.compile("^\\(\\d{3}\\) \\d{3}-\\d{4}$|^\\d{3}-\\d{3}-\\d{4}$");
            String line;
            while ((line = br.readLine()) != null) {
                if (pattern.matcher(line).matches()) {
                    System.out.println(line);
                }
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }

    // Explanation:
    // Time Complexity: O(n) - where n is the number of lines in file.txt.
    // Space Complexity: O(1) - apart from input file reading, no additional data structures are used.
}
```

```javascript
// JavaScript
// Read from the file file.txt and output all valid phone numbers to stdout.

const fs = require('fs');
const readline = require('readline');

function readValidPhoneNumbers() {
    const fileStream = fs.createReadStream('file.txt');

    const rl = readline.createInterface({
        input: fileStream,
        crlfDelay: Infinity
    });

    const regex = /^\(\d{3}\) \d{3}-\d{4}$|^\d{3}-\d{3}-\d{4}$/;

    rl.on('line', (line) => {
        if (regex.test(line)) {
            console.log(line);
        }
    });
}

// Explanation:
// Time Complexity: O(n) - where n is the number of lines in file.txt.
// Space Complexity: O(1) - apart from reading file input, no extra memory is needed.

readValidPhoneNumbers();
```

```cpp
// C++
// Read from the file file.txt and output all valid phone numbers to stdout.

#include <iostream>
#include <fstream>
#include <regex>

void readValidPhoneNumbers() {
    std::ifstream file("file.txt");
    std::string line;
    std::regex pattern(R"(^\(\d{3}\) \d{3}-\d{4}$|^\d{3}-\d{3}-\d{4}$)");

    if (file.is_open()) {
        while (std::getline(file, line)) {
            if (std::regex_match(line, pattern)) {
                std::cout << line << std::endl;
            }
        }
        file.close();
    }
}

// Explanation:
// Time Complexity: O(n) - where n is the number of lines in file.txt.
// Space Complexity: O(1) - no extra data structures are used other than the input file.

int main() {
    readValidPhoneNumbers();
    return 0;
}
```

### Time and Space Complexity Recap:

For all the solutions provided across different programming languages, the complexities are:

- **Time Complexity:** O(n) where n is the number of lines in `file.txt`. This is because each line needs to be scanned to check if it matches the required format.
- **Space Complexity:** O(1) since we are not using any additional data structures to store lines from the file; processing is done directly.


### Closing Statement:

**Interviewer:**
Thank you for providing a comprehensive solution across multiple programming languages. You've done a great job explaining the time and space complexities and showed a deep understanding of using regular expressions to filter valid phone numbers efficiently. The one-liner bash script was particularly well-executed for this problem.

**Interviewee:**
Thank you! It was a great exercise in understanding how to handle file input and pattern matching across different languages. I'm glad I could demonstrate the use of regular expressions efficiently.

**Interviewer:**
It’s also good to see that you could translate a single logic across different programming languages while maintaining optimal time and space complexity. Keep practicing, and you will keep getting better at identifying and implementing these patterns.

### Similar Questions:

1. **Email Address Validation:** 
   - Given a text file `emails.txt` that contains a list of email addresses (one per line), write a one-liner bash script to print all valid email addresses. A valid email address should follow the format `local-part@domain`.

2. **URL Extraction:** 
   - Given a text file `urls.txt` containing URLs (one per line), write a script to extract and print the valid URLs. Consider a valid URL to be one that starts with `http://` or `https://`.

3. **Log File Analysis:**
   - Given a server log file `log.txt`, write a script to extract and print all the lines that contain 404 status codes.

4. **IPv4 Address Validation:**
   - Write a script to read from a file `ips.txt` and output all valid IPv4 addresses. A valid IPv4 address must be in the format `xxx.xxx.xxx.xxx` where `xxx` is a number between 0 and 255.

5. **Extract Dates:**
   - Given a text file `dates.txt` that contains different date formats, write a script to extract and print all dates in the format `YYYY-MM-DD`.

6. **Identifying Subdomains:**
   - Given a text file `domains.txt` that contains a list of domain names (one per line), write a script to identify and print all subdomains of a given base domain.

By practicing these similar questions, you'll gain a deeper understanding of file handling, pattern matching using regular expressions, and efficiently solving real-world string processing tasks in various programming languages.