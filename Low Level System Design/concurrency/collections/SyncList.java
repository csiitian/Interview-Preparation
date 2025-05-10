package collections;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Queue;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.CopyOnWriteArrayList;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class SyncList {
    public static void main(String[] args) {
        List<Integer> syncList = Collections.synchronizedList(new ArrayList<>());
        List<Integer> normalList = new ArrayList<>();
        List<Integer> copyOnWriteList = new CopyOnWriteArrayList<>();
        Queue<Integer> concurrentQueue = new ConcurrentLinkedQueue<>();
        Runnable runnable = () -> {
            for (int i = 0; i < 10000; i++) {
                syncList.add(i);
                normalList.add(i);
                copyOnWriteList.add(i);
                concurrentQueue.add(i);
            }
            System.out.println("Done");
        };
        try (ExecutorService executorService = Executors.newFixedThreadPool(10)) {
            for (int i = 0; i < 10; i++) {
                executorService.execute(runnable);
            }
        }
        System.out.println("SyncList size: " + syncList.size());
        System.out.println("NormalList size: " + normalList.size());
        System.out.println("CopyOnWriteList size: " + copyOnWriteList.size());
        System.out.println("ConcurrentQueue.size: " + concurrentQueue.size());
    }
}
