package collections;

import java.util.*;

public class TreeMapTest {

    public static void main(String[] args) {
        TreeMap<Integer, List<Integer>> map = new TreeMap<>(Collections.reverseOrder());
        map.put(1, Arrays.asList(100));
        map.put(-10, Arrays.asList(200));
        map.put(10, Arrays.asList(100));

        System.out.println(map);

        List<Integer> val = map.pollLastEntry().getValue();
        System.out.println(val);
    }
}
