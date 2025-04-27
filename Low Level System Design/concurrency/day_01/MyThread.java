package concurrency.day_01;

public class MyThread extends Thread {
    public void run() {
        System.out.println(Thread.currentThread().getName() + ": extending Thread Class...");
    }
}
