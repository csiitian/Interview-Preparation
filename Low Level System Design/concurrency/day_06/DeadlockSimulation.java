package concurrency.day_06;

public class DeadlockSimulation {
    Object lock1 = new Object();
    Object lock2 = new Object();

    void processA() {
        synchronized (lock1) {
            System.out.println(Thread.currentThread().getName() + " got lock1");
            synchronized (lock2) {
                System.out.println(Thread.currentThread().getName() + " got lock2");
            }
        }
    }

    void processB() {
        synchronized (lock2) {
            System.out.println(Thread.currentThread().getName() + " got lock2");
            synchronized (lock1) {
                System.out.println(Thread.currentThread().getName() + " got lock1");
            }
        }
    }
}
