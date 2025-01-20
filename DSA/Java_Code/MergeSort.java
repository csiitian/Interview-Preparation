package DSA.Java_Code;

public class MergeSort {

  public static void main(String[] args) {
    int[] arr = {10, 100, 90, 54, -100, 1, 1, 10001, -12, 0, -1023};
    int n = arr.length;
    mergeSort(arr, 0, n - 1);
    printArray(arr);
  }

  private static void mergeSort(int[] arr, int start, int end) {
    if (start >= end) {
      return;
    }
    int mid = start + (end - start) / 2;
    mergeSort(arr, start, mid);
    mergeSort(arr, mid + 1, end);
    merge(arr, start, mid, end);
  }

  private static void merge(int[] arr, int start, int mid, int end) {
    int n1 = mid - start + 1;
    int n2 = end - mid;
    int[] left = new int[n1];
    int[] right = new int[n2];
    for(int i=start;i<=mid;i++) left[i-start] = arr[i];
    for(int i=mid+1;i<=end;i++) right[i-mid-1] = arr[i];

    int i = 0, j = 0, k = start;
    while(i < n1 && j < n2) {
      if(left[i] <= right[j]) {
        arr[k++] = left[i++];
      } else {
        arr[k++] = right[j++];
      }
    }
    while(i < n1) arr[k++] = left[i++];
    while(j < n2) arr[k++] = right[j++];
  }

  private static void printArray(int[] arr) {
    for (int x : arr) {
      System.out.print(x + " ");
    }
    System.out.println();
  }
}
