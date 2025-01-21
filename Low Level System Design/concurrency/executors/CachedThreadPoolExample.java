package concurrency.executors;

import java.time.Duration;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class CachedThreadPoolExample {

    public static void main(String[] args) {
        long startTime = System.currentTimeMillis();
        try (ExecutorService executorService = Executors.newCachedThreadPool()) {
            for (int i = 0; i < 2000; i++) {
                executorService.submit(CachedThreadPoolExample::processTask);
            }
        }
        long endTime = System.currentTimeMillis();
        System.out.println("Time: " + (endTime - startTime) / 1000 + " sec.");

        startTime = System.currentTimeMillis();
        for (int i = 0; i < 1; i++) {
            processTask();
        }
        endTime = System.currentTimeMillis();
        System.out.println("Time: " + (endTime - startTime) / 1000 + " sec.");
    }

    private static void processTask() {
        try {
            Thread.sleep(Duration.ofSeconds(5));
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
