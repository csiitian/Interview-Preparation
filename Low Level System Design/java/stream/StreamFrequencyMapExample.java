package stream;

import java.util.Arrays;
import java.util.function.Function;
import java.util.stream.Collectors;

public class StreamFrequencyMapExample {

  public static void main(String[] args) {

    int[] array = {12, 12, 7, 12, 2, 2, 3, 7};
    Arrays.stream(array)
        .boxed()
        .collect(Collectors.groupingBy(Function.identity(), Collectors.counting()))
        .entrySet()
        .forEach(System.out::println);
  }
}
