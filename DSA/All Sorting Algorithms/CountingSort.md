# Counting Sort

Counting Sort is a non-comparison-based sorting algorithm. It works by counting the number of occurrences of each unique element in the input array. The count is stored in an auxiliary array, and the sorting is done by calculating the position of each element in the output array.

## C++

```cpp
#include <iostream>
#include <vector>
using namespace std;

void countSort(int arr[], int n) {
    int max = *max_element(arr, arr + n);
    int min = *min_element(arr, arr + n);
    int range = max - min + 1;

    vector<int> count(range), output(n);
    for (int i = 0; i < n; i++)
        count[arr[i] - min]++;

    for (int i = 1; i < count.size(); i++)
        count[i] += count[i - 1];

    for (int i = n - 1; i >= 0; i--) {
        output[count[arr[i] - min] - 1] = arr[i];
        count[arr[i] - min]--;
    }

    for (int i = 0; i < n; i++)
        arr[i] = output[i];
}

void printArray(int arr[], int n) {
    for (int i = 0; i < n; i++)
        cout << arr[i] << " ";
    cout << "\n";
}

int main() {
    int arr[] = {4, 2, 2, 8, 3, 3, 1};
    int n = sizeof(arr) / sizeof(arr[0]);
    countSort(arr, n);
    printArray(arr, n);
    return 0;
}
```

## Python

```python
def countSort(arr):
    max_element = max(arr)
    min_element = min(arr)
    range_of_elements = max_element - min_element + 1

    count_arr = [0] * range_of_elements
    output_arr = [0] * len(arr)

    for i in range(len(arr)):
        count_arr[arr[i] - min_element] += 1

    for i in range(1, len(count_arr)):
        count_arr[i] += count_arr[i - 1]

    for i in range(len(arr) - 1, -1, -1):
        output_arr[count_arr[arr[i] - min_element] - 1] = arr[i]
        count_arr[arr[i] - min_element] -= 1

    for i in range(len(arr)):
        arr[i] = output_arr[i]

arr = [4, 2, 2, 8, 3, 3, 1]
countSort(arr)
print("Sorted array is:", arr)
```

## Java

```java
public class CountingSort {
    void countSort(int arr[]) {
        int n = arr.length;

        int max = arr[0];
        int min = arr[0];
        for (int i = 1; i < n; i++) {
            if (arr[i] > max)
                max = arr[i];
            if (arr[i] < min)
                min = arr[i];
        }

        int range = max - min + 1;
        int count[] = new int[range];
        int output[] = new int[n];

        for (int i = 0; i < n; i++)
            count[arr[i] - min]++;

        for (int i = 1; i < count.length; i++)
            count[i] += count[i - 1];

        for (int i = n - 1; i >= 0; i--) {
            output[count[arr[i] - min] - 1] = arr[i];
            count[arr[i] - min]--;
        }

        for (int i = 0; i < n; i++)
            arr[i] = output[i];
    }

    static void printArray(int arr[]) {
        int n = arr.length;
        for (int i = 0; i < n; i++)
            System.out.print(arr[i] + " ");
        System.out.println();
    }

    public static void main(String args[]) {
        int arr[] = {4, 2, 2, 8, 3, 3, 1};
        CountingSort ob = new CountingSort();
        ob.countSort(arr);
        printArray(arr);
    }
}
```