#include <iostream>
#include <queue>
using namespace std;

int main() {
  int t;
  cin >> t;
  while(t--) {
    int n, m;
    cin >> n >> m;
    int a[n][m];
    for(int i=0;i<n;i++) {
      for(int j=0;j<m;j++) {
        cin >> a[i][j];
      }
    }

    int s = 1;
    int d = -1;
    int b[n][m];
    bool possible = true;
    for(int i=0;i<n;i++) {
      for(int j=0;j<m;j++) {
        if(d != -1 && a[i][j] != d) {
          b[i][j] = d;
          d = -1;
        } else if(a[i][j] != s) {
          b[i][j] = s++;
        } else {
          d = s;
          b[i][j] = (s+1);
          s += 2;
        }

        if(b[i][j] > (n*m)) possible = false;
      }
    }

    if(!possible) {
      if(b[n-1][m-1] == (n*m + 1)) {
        b[n-1][m-1]--;
        int temp = b[n-1][m-1];
        b[n-1][m-1] = b[0][0];
        b[0][0] = temp;

        if(a[0][0] == b[0][0] || a[n-1][m-1] == b[n-1][m-1])
          possible = false;
        else
          possible = true;
      }
    }

    if(possible) {
      for(int i=0;i<n;i++) {
        for(int j=0;j<m;j++) {
          cout << b[i][j] << " ";
        }
        cout << endl;
      }
    } else {
      cout << "-1" << endl;
    }
  }
  return 0;
}