package collections;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

public class SynchronizedList {

    public static void main(String[] args) {
        List<Integer> list = Collections.synchronizedList(new ArrayList<>());
        list.add(1);
        list.add(2);
        for (int x: list) {
            System.out.println(x);
        }
    }
}
