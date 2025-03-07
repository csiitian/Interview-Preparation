package concurrency.executors;

import java.time.Duration;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class FixedThreadPoolExample {

    public static void main(String[] args) {
        long startTime = System.currentTimeMillis();
        try (ExecutorService executorService = Executors.newFixedThreadPool(4)) {
            for (int i = 0; i < 10; i++) {
                executorService.submit(FixedThreadPoolExample::processTask);
            }
        }
        long endTime = System.currentTimeMillis();
        System.out.println("Time: " + (endTime - startTime) / 1000 + " sec.");

        startTime = System.currentTimeMillis();
        for (int i = 0; i < 10; i++) {
            processTask();
        }
        endTime = System.currentTimeMillis();
        System.out.println("Time: " + (endTime - startTime) / 1000 + " sec.");
    }

    private static void processTask() {
        try {
            Thread.sleep(Duration.ofSeconds(1));
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
