package concurrency.day_08;

import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class SharedPrinterUsingLock {
    static volatile int count = 0;
    Lock lock = new ReentrantLock();
    Condition evenCondition = lock.newCondition();
    Condition oddCondition = lock.newCondition();

    void printEven() {
        lock.lock();
        try {
            while(count < 10) {
                while (count % 2 == 1) {
                    evenCondition.await();
                }
                System.out.println(Thread.currentThread().getName() + ": " + count++);
                oddCondition.signal();
            }
        } catch (InterruptedException e) {
            e.printStackTrace();
        } finally {
            System.out.println(Thread.currentThread().getName() + ": releasing lock");
            lock.unlock();
        }
    }

    void printOdd() {
        lock.lock();
        try {
            while(count < 10) {
                while (count % 2 == 0) {
                    oddCondition.await();
                }
                System.out.println(Thread.currentThread().getName() + ": " + count++);
                evenCondition.signal();
            }
        } catch (InterruptedException e) {
            e.printStackTrace();
        } finally {
            System.out.println(Thread.currentThread().getName() + ": releasing lock");
            lock.unlock();
        }
    }
}

/*
Theory:

Thread must hold the lock before calling await(). (Otherwise IllegalMonitorStateException)

Inside await():
The thread: Releases the lock temporarily (internally).
Moves to waiting state (parked by the JVM, not running anymore).
Other threads can now acquire the lock because the first thread released it inside await().

When evenCondition.signal() happens from another thread:
The waiting thread is notified.
The notified thread tries to re-acquire the lock.
Once it re-acquires the lock, it continues from after the await() call.

 */
