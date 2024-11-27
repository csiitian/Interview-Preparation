package stream;

import java.util.stream.Stream;

public class StreamExample {

  public static void main(String[] args) {

    Stream<String> names = Stream.of("Vishal", "Satish", "Kapil", "Sachin");

    names.filter(name -> name.contains("i"))
        .map(String::toUpperCase)
        .sorted()
        .forEach(System.out::println);
  }
}
