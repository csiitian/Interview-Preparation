package stream;

import java.util.stream.IntStream;

public class StreamMathematicalExpressionExample {

  public static void main(String[] args) {

    double pi = Math.sqrt(12) *
        IntStream.rangeClosed(0, 100)
            .mapToDouble(k -> Math.pow(-3, -1 * k) / (2 * k + 1))
            .sum();

    System.out.println(pi);
  }
}
