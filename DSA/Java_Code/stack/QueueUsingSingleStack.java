package DSA.Java_Code.stack;

import java.util.Stack;

/**
 * either use enQueue and deQueue together
 * or enQueue_v2 and deQueue_v2 together
 */
public class QueueUsingSingleStack {

  Stack<Integer> stack;

  public QueueUsingSingleStack() {
    stack = new Stack<>();
  }

  /**
   * @param data
   * enQueue is O(1)
   */
  public void enQueue(int data) {
    stack.push(data);
  }

  /**
   * deQueue is O(n)
   * @return data inserted first ( FIFO )
   */
  public int deQueue() {
    if (stack.isEmpty()) return -1;
    else if(stack.size() == 1) return stack.pop();
    else {
      int pop = stack.pop();
      int ans = deQueue();
      stack.push(pop);
      return ans;
    }
  }

  /**
   * enQueue_v2 is O(n)
   * @param data
   */
  public void enQueue_v2(int data) {
    if (stack.isEmpty()) stack.push(data);
    else {
      int pop = stack.pop();
      enQueue_v2(data);
      stack.push(pop);
    }
  }

  /**
   * deQueue_v2 is O(1)
   * @return data inserted first ( FIFO )
   */
  public int deQueue_v2() {
    if (stack.isEmpty()) return -1;
    return stack.pop();
  }

  // driver method
  public static void main(String[] args) {
    QueueUsingSingleStack queue = new QueueUsingSingleStack();
    queue.enQueue_v2(10);
    queue.enQueue_v2(20);
    queue.enQueue_v2(30);

    System.out.println(queue.deQueue_v2());
    System.out.println(queue.deQueue_v2());
    System.out.println(queue.deQueue_v2());
  }
}
