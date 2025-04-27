package concurrency.day_11;

import java.util.concurrent.Semaphore;

public class SemaphoreExample {
    Semaphore semaphore;

    SemaphoreExample(int n) {
        semaphore = new Semaphore(n);
    }

    // This method can be executed by n threads consecutively
    void execute() {
        try {
            semaphore.acquire();
            System.out.println(Thread.currentThread().getName() + " is running " + System.currentTimeMillis() / 1000);
            Thread.sleep(5000);
            semaphore.release();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
