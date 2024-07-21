#include <iostream>
#include <queue>
using namespace std;
 
// Main() function: where the execution of
// program begins
int main() {
    // Prints hello world
    int t;
    cin >> t;
    while(t-- > 0) {
        int a, b, c;
        cin >> a >> b >> c;

        priority_queue<int, vector<int>, greater<int> > pq;
        pq.push(a);
        pq.push(b);
        pq.push(c);

        int d = 5;
        while(d--) {
            int top = pq.top();
            pq.pop();
            pq.push(top+1);
        }

        a = pq.top();
        pq.pop();
        b = pq.top();
        pq.pop();
        c = pq.top();

        int ans = a * b * c;
        cout << ans << endl;
    }
    return 0;
}