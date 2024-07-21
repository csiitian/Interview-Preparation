#include <iostream>
#include <queue>
using namespace std;

int main() {
  int t;
  cin >> t;
  while(t-- > 0) {
    int n;
    cin >> n;
    string s, t;
    cin >> s >> t;
    bool ans = true;
    for(int i=0;i<n;i++) {
      if(s[i] == '1') break;
      if(t[i] == '1') {
        ans = false;
        break;
      }
    }
    if(ans) cout << "YES" << endl;
    else cout << "NO" << endl;
  }
  return 0;
}