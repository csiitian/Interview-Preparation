package concurrency.day_12;

import java.util.concurrent.CountDownLatch;

public class MainApplication {
    public static void main(String[] args) {
        int n = 1;
        CountDownLatch countDownLatch = new CountDownLatch(n);
        Worker worker = new Worker(countDownLatch);
        Thread[] threads = new Thread[10];
        for (int i = 0; i < threads.length; i++) {
            threads[i] = new Thread(worker);
            try {
                Thread.sleep(10); // adding some sleep time between consecutive work
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
            threads[i].start();
        }
        try {
            countDownLatch.await(); // it will wait for n workers to complete the task
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        System.out.println("Exiting...");
    }
}
