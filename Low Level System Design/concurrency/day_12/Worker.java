package concurrency.day_12;

import java.util.concurrent.CountDownLatch;

public class Worker implements Runnable {
    final CountDownLatch countDownLatch;

    Worker(CountDownLatch countDownLatch) {
        this.countDownLatch = countDownLatch;
    }

    @Override
    public void run() {
        try {
            Thread.sleep(5000);
            System.out.println(Thread.currentThread().getName() + "<UNK>");
        } catch (InterruptedException e) {
            e.printStackTrace();
        } finally {
            countDownLatch.countDown();
        }
    }
}
