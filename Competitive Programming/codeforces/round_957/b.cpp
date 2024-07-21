#include <iostream>
#include <queue>
using namespace std;
 
// Main() function: where the execution of
// program begins
int main() {

  int t;
  cin >> t;
  while(t--) {
    int n, k;
    cin >> n >> k;
    int arr[k];
    int maxI = 0;
    for(int i=0;i<k;i++) {
      cin >> arr[i];
      if(arr[i] > arr[maxI]) maxI = i;
    }
    long int ans = 0l;
    for(int i=0;i<k;i++) {
      if(maxI != i) {
        if(arr[i] == 1) {
          ans += 1l;
        } else if(arr[i] > 1) {
          ans += 2 * (arr[i]) - 1;
        } 
      }
    }

    cout << ans << endl;
  }
  return 0;
}