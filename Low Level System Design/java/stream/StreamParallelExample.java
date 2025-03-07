package stream;

import java.util.stream.IntStream;

public class StreamParallelExample {

  public static void main(String[] args) {
    IntStream stream = IntStream.iterate(1, x -> x + 1);
    long startTime = System.currentTimeMillis();
    stream.parallel().limit(100000).forEach(System.out::print);
    long endTime = System.currentTimeMillis();
    System.out.println("\n Parallel Stream processed in " + (endTime - startTime));

    stream = IntStream.iterate(1, x -> x + 1);
    startTime = System.currentTimeMillis();
    stream.limit(100000).forEach(System.out::print);
    endTime = System.currentTimeMillis();
    System.out.println("\n Stream processed in " + (endTime - startTime));
  }
}
