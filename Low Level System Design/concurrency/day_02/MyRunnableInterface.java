package concurrency.day_02;

public class MyRunnableInterface implements Runnable {
    @Override
    public void run() {
        System.out.println(Thread.currentThread().getName() + ": implementing Runnable Interface...");
    }
}
