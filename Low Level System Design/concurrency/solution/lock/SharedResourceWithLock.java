package concurrency.solution.lock;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class SharedResourceWithLock {
    int count = 0;
    private Lock lock;

    SharedResourceWithLock() {
        lock = new ReentrantLock();
    }

    void increment() {
        try {
            lock.lock();
            for (int i = 0; i < 1000000; i++) {
                count++;
//                System.out.println(Thread.currentThread().getName());
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
        Thread thread3 = new Thread(sharedRunnable, "Thread-3");
        Thread thread4 = new Thread(sharedRunnable, "Thread-4");
        Thread thread5 = new Thread(sharedRunnable, "Thread-5");
        thread1.start();
        thread2.start();
        thread3.start();
        thread4.start();
        thread5.start();
        try {
            thread1.join();
            thread2.join();
            thread3.join();
            thread4.join();
            thread5.join();
        } catch (InterruptedException e) {
            System.out.println(e.getMessage());
        }
        long endTimestamp = System.currentTimeMillis();
        System.out.println(sharedResource.count + " " + ((endTimestamp - startTimestamp) / 1000d));
    }
}