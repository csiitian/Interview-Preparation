package DSA.Java_Code.stack;

import java.util.Stack;

public class PostfixToInfixConverter implements IConvertor {

  static class InvalidExpression extends RuntimeException {
    InvalidExpression() {
      super("Invalid Expression.");
    }
  }

  PostfixToInfixConverter() {
  }

  @Override
  public String convert(String expression) {
    Stack<String> stack = new Stack<>();

    for (char ch: expression.toCharArray()) {
      if (Character.isAlphabetic(ch)) {
        stack.push(String.valueOf(ch));
      } else {
        if (stack.size() < 2) {
          throw new InvalidExpression();
        }
        String popA = stack.pop();
        String popB = stack.pop();
        stack.push("(" + popB + ch + popA + ")");
      }
    }

    if (stack.size() != 1) {
      throw new InvalidExpression();
    }

    return stack.pop();
  }

  // driver method
  public static void main(String[] args) {
    InfixToPostfixConverter infixToPostfixConverter = new InfixToPostfixConverter();
    PostfixToInfixConverter postfixToInfixConverter = new PostfixToInfixConverter();

    String postfix = infixToPostfixConverter.convert("((A/B)*(C-D*F)+G)");
    String infix = postfixToInfixConverter.convert(postfix);

    System.out.println(infix);
  }
}
