package DSA.Java_Code;

public class QuickSort {

  public static void main(String[] args) {

    int[] arr = {10, 100, 90, 54, -100, 1, 1, 10001, -12, 0, -1023};
    int n = arr.length;
    quickSortStart2(arr, 0, n - 1);
    printArray(arr);
  }

  /**
   * Quick Sort, Selecting Pivot as last element
   * @param arr
   * @param start
   * @param end
   */
  private static void quickSortEnd(int[] arr, int start, int end) {
    if (start > end) {
      return;
    }
    int pivot = end;
    int i = start;
    for (int j = start; j < end; j++) {
      if (arr[j] < arr[pivot]) {
        swap(arr, i, j);
        i++;
      }
    }
    swap(arr, i, pivot);
    quickSortEnd(arr, start, i - 1);
    quickSortEnd(arr, i + 1, end);
  }

  /**
   * Quick Sort, Selecting Pivot as first element
   * @param arr
   * @param start
   * @param end
   */
  private static void quickSortStart(int[] arr, int start, int end) {
    if (start > end) {
      return;
    }
    int pivot = start;
    int i = start + 1;
    for (int j = start + 1; j <= end; j++) {
      if (arr[j] < arr[pivot]) {
        swap(arr, i, j);
        i++;
      }
    }
    swap(arr, i - 1, pivot);
    quickSortStart(arr, start, i - 2);
    quickSortStart(arr, i, end);
  }

  /**
   * Quick Sort, Selecting Pivot as first element
   * @param arr
   * @param start
   * @param end
   */
  private static void quickSortStart2(int[] arr, int start, int end) {
    if (start > end) {
      return;
    }
    int pivot = start;
    int i = end;
    for (int j = end; j > start; j--) {
      if (arr[j] > arr[pivot]) {
        swap(arr, i, j);
        i--;
      }
    }
    swap(arr, i, pivot);
    quickSortStart2(arr, start, i - 1);
    quickSortStart2(arr, i + 1, end);
  }

  public static void printArray(int[] arr) {
    for (int x : arr) {
      System.out.print(x + " ");
    }
    System.out.println();
  }

  public static void swap(int[] arr, int i, int j) {
    int temp = arr[i];
    arr[i] = arr[j];
    arr[j] = temp;
  }
}
