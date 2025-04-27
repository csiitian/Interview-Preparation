package concurrency.day_2;

public class MyRunnableInterface implements Runnable {
    @Override
    public void run() {
        System.out.println(Thread.currentThread().getName() + ": implementing Runnable Interface...");
    }
}
