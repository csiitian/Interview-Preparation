package stream;

import java.util.stream.IntStream;

public class StreamInfiniteExample {

  public static void main(String[] args) {
    IntStream intStream = IntStream.iterate(1, x -> x + 1);
        //.limit(10);
    intStream.forEach(System.out::println);
  }
}
