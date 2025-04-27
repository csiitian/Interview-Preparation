package collections;

import java.util.HashMap;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class MapBenchMark {
    public static void main(String[] args) {
        ConcurrentHashMap<Integer, Integer> concurrentHashMap = new ConcurrentHashMap<>();
        long startTime = System.currentTimeMillis();
        ExecutorService executorService = Executors.newFixedThreadPool(10);
        for (int i = 0; i < (int) 1e7; i++) {
            int finalI = i;
            executorService.execute(() -> {
                concurrentHashMap.put(finalI, finalI);
            });
        }
        long endTime = System.currentTimeMillis();
        System.out.println((endTime - startTime));

        HashMap<Integer, Integer> hashMap = new HashMap<>();
        startTime = System.currentTimeMillis();
        for (int i = 0; i < (int) 1e7; i++) {
            int finalI = i;
            executorService.execute(() -> {
                hashMap.put(finalI, finalI);
            });
        }
        endTime = System.currentTimeMillis();
        System.out.println((endTime - startTime));
    }
}
