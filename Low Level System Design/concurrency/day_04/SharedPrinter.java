package concurrency.day_04;

public class SharedPrinter {
    static volatile int counter = 1;

    synchronized void printEven() {
        System.out.println(Thread.currentThread().getName());
        for (int i = 1; i < 1000; i++) {
            if (i % 2 == 0) {
                System.out.println(Thread.currentThread().getName() + ": " + counter++);
                notifyAll();
            } else {
                try {
                    wait();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }
    }

    synchronized void printOdd() {
        for (int i = 1; i < 1000; i++) {
            if (i % 2 == 1) {
                System.out.println(Thread.currentThread().getName() + ": " + counter++);
                notifyAll();
            } else {
                try {
                    wait();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
        }
    }
}