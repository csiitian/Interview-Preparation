package concurrency.executors;

import java.time.LocalDateTime;
import java.util.Random;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

public class ScheduledThreadPoolExample {

    public static void main(String[] args) {
        ScheduledExecutorService executorService = Executors.newScheduledThreadPool(2);
        executorService.scheduleAtFixedRate(() -> processTask("Task#1"), 0, 1, TimeUnit.SECONDS);
        executorService.scheduleAtFixedRate(() -> processTask("Task#2"), 0, 1, TimeUnit.SECONDS);
        executorService.scheduleAtFixedRate(() -> processTask("Task#3"), 0, 1, TimeUnit.SECONDS);
//        executorService.scheduleWithFixedDelay(ScheduledThreadPoolExample::processTask, 0, 1, TimeUnit.SECONDS);
//        executorService.scheduleWithFixedDelay(ScheduledThreadPoolExample::processTask, 0, 1, TimeUnit.SECONDS);
//        executorService.scheduleWithFixedDelay(ScheduledThreadPoolExample::processTask, 0, 1, TimeUnit.SECONDS);
        Runtime.getRuntime().addShutdownHook(new Thread(() -> {
            executorService.shutdownNow();
            System.out.println("Shutting down");
        }));
    }

    private static void processTask(String taskName) {
        System.out.println("Started: " + "[" + taskName +  "]" + Thread.currentThread().threadId() + " " + LocalDateTime.now());
        int i = 0;
        while(i < (int) 1e7) {
            new Random().nextInt();
            i++;
        }
//        System.out.println("Completed: " + "[" + taskName +  "]" + Thread.currentThread().threadId() + " " + LocalDateTime.now());
    }
}
