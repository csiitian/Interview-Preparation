package DSA.Java_Code.stack;

import java.util.LinkedList;
import java.util.Queue;
import java.util.Stack;

public class StackUsingSingleQueue<T> {
  Queue<T> queue;

  StackUsingSingleQueue() {
    queue = new LinkedList<>();
  }

  public T push(T item) {
    queue.add(item);
    return item;
  }

  public T pop() {
    if (queue.isEmpty()) return null;
    else if(queue.size() == 1) return queue.poll();
    else {
      T poll = queue.poll();
      T ans = pop();
      queue.add(poll);
      return ans;
    }
  }

  // driver method
  public static void main(String[] args) {
    StackUsingSingleQueue<Integer> stack = new StackUsingSingleQueue<>();
    stack.push(10);
    stack.push(20);
    System.out.println(stack.pop());
    System.out.println(stack.pop());
  }
}
