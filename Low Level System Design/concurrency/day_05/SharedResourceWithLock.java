package concurrency.day_05;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class SharedResourceWithLock {
    int count = 0;
    private Lock lock;

    SharedResourceWithLock() {
        lock = new ReentrantLock(true); // fairness
    }

    void increment() {
        try {
            lock.lock();
            for (int i = 0; i < 10000; i++) {
                count++;
            }
        } finally{
            lock.unlock();
        }
    }

    public static void main(String[] args) {
        long startTimestamp = System.currentTimeMillis();
        SharedResourceWithLock sharedResource = new SharedResourceWithLock();
        Runnable sharedRunnable = sharedResource::increment;
        Thread thread1 = new Thread(sharedRunnable, "Thread-1");
        Thread thread2 = new Thread(sharedRunnable, "Thread-2");
        thread1.start();
        thread2.start();
        try {
            thread1.join();
            thread2.join();
        } catch (InterruptedException e) {
            System.out.println(e.getMessage());
        }
        long endTimestamp = System.currentTimeMillis();
        System.out.println(sharedResource.count + " " + ((endTimestamp - startTimestamp) / 1000d));
    }
}