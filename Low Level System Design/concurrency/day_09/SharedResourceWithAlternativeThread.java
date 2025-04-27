package concurrency.day_09;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class SharedResourceWithAlternativeThread {
    int count = 0;
    Lock lock;
    Condition condition;
    int threadIndex = 0;
    int totalThreads = 5;

    public SharedResourceWithAlternativeThread() {
        lock = new ReentrantLock();
        condition = lock.newCondition();
    }

    void increment(int threadIndex) {
        try {
            lock.lock();
            for (int i = 0; i < 10; i++) {
                while (this.threadIndex != threadIndex) {
                    condition.await();
                }
                count++;
                System.out.println(threadIndex);
                this.threadIndex = (this.threadIndex + 1) % 5;
                condition.signalAll();
            }
        } catch (InterruptedException e) {
            System.out.println(e.getMessage());
        } finally {
            lock.unlock();
        }
    }

    public static void main(String[] args) {
        SharedResourceWithAlternativeThread sharedResource = new SharedResourceWithAlternativeThread();
        Thread[] threads = new Thread[sharedResource.totalThreads];

        for (int i = 0; i < threads.length; i++) {
            int finalThreadIndex = i;
            threads[i] = new Thread(() -> sharedResource.increment(finalThreadIndex));
            threads[i].start();
        }

        for (Thread thread: threads) {
            try {
                thread.join();
            } catch (InterruptedException e) {
                System.out.println(e.getMessage());
            }
        }

        System.out.println(sharedResource.count);
    }
}