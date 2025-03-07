package concurrency.thread;

public class MyRunnableInterface implements Runnable {
    @Override
    public void run() {
        System.out.println("Runnable is running...");
    }
}
