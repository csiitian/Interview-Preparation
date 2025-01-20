package DSA.Java_Code.stack;

public class TwoStackInOneArray {
  int[] stack;
  int top1, top2, size;

  TwoStackInOneArray(int size) {
    stack = new int[size];
    top1 = -1;
    top2 = size;
    this.size = size;
  }

  boolean isStackOneEmpty() {
    return top1 == -1;
  }

  boolean isStackTwoEmpty() {
    return top1 == size;
  }

  boolean isFull() {
    return top2 - top1 <= 1;
  }

  void pushInStackOne(int data) {
    if (isFull()) {
      throw new FullStackException();
    }
    stack[++top1] = data;
  }

  int popFromStackOne() {
    if (isStackOneEmpty()) {
      throw new EmptyStackException();
    }
    return stack[top1--];
  }

  void pushInStackTwo(int data) {
    if (isFull()) {
      throw new FullStackException();
    }
    stack[--top2] = data;
  }

  int popFromStackTwo() {
    if (isStackTwoEmpty()) {
      throw new EmptyStackException();
    }
    return stack[top2++];
  }

  // driver method
  public static void main(String[] args) {
    TwoStackInOneArray stack = new TwoStackInOneArray(3);
    try {
      stack.pushInStackOne(10);
      System.out.println("10 pushed to stack1");
    } catch (Exception e) {
      System.out.println(e.getMessage());
    }
    try {
      stack.pushInStackOne(20);
      System.out.println("20 pushed to stack1");
    } catch (Exception e) {
      System.out.println(e.getMessage());
    }
    try {
      stack.pushInStackOne(30);
      System.out.println("30 pushed to stack1");
    } catch (Exception e) {
      System.out.println(e.getMessage());
    }
    try {
      stack.pushInStackTwo(10);
      System.out.println("10 pushed to stack2");
    } catch (Exception e) {
      System.out.println(e.getMessage());
    }
    try {
      int pop = stack.popFromStackOne();
      System.out.println(pop + " popped from stack1");
    } catch (Exception e) {
      System.out.println(e.getMessage());
    }
    try {
      stack.pushInStackTwo(20);
      System.out.println("20 pushed to stack2");
    } catch (Exception e) {
      System.out.println(e.getMessage());
    }
  }
}

class FullStackException extends RuntimeException {
  FullStackException() {
    super("Stack is full.");
  }
  FullStackException(String message) {
    super(message);
  }
}

class EmptyStackException extends RuntimeException {
  EmptyStackException() {
    super("Stack is empty.");
  }
  EmptyStackException(String message) {
    super(message);
  }
}
