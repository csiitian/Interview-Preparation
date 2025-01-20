package DSA.Java_Code.stack;

import java.util.Stack;

class InfixToPostfixConverter implements IConvertor {

  static class InvalidExpression extends RuntimeException {
    InvalidExpression() {
      super("Invalid Expression.");
    }
  }

  InfixToPostfixConverter() {
  }

  public int getOperatorPrecedence(char op) {
    return switch (op) {
      case '+', '-' -> 1;
      case '*', '/' -> 2;
      default -> 0;
    };
  }

  @Override
  public String convert(String expression) {
    Stack<Character> stack = new Stack<>();
    StringBuilder postfixExp = new StringBuilder();

    for (char ch: expression.toCharArray()) {
      if (Character.isLetterOrDigit(ch)) {
        postfixExp.append(ch);
      } else if (ch == '(') {
        stack.push(ch);
      } else if (ch == ')') {
        while (!stack.isEmpty() && stack.peek() != '(') {
          postfixExp.append(stack.pop());
        }
        if (stack.isEmpty()) {
          throw new InvalidExpression();
        }
        stack.pop(); // pop the (
      } else {
        // check the operator precedence
        while (!stack.isEmpty() && stack.peek() != '(' && getOperatorPrecedence(stack.peek()) >= getOperatorPrecedence(ch)) {
          postfixExp.append(stack.pop());
        }
        stack.push(ch);
      }
    }

    while(!stack.isEmpty() && stack.peek() != '(') {
      postfixExp.append(stack.pop());
    }

    if (!stack.isEmpty()) {
      throw new InvalidExpression();
    }

    return postfixExp.toString();
  }

  // driver method
  public static void main(String[] args) {
    InfixToPostfixConverter converter = new InfixToPostfixConverter();
    System.out.println(converter.convert("A+B*C+D"));
    System.out.println(converter.convert("((A+B)–C*(D/E))+F"));
    System.out.println(converter.convert("3*4-2*5"));
  }
}
