package concurrency;

import concurrency.thread.MyRunnableInterface;
import concurrency.thread.MyThread;

public class MainApplication {

    public static void main(String[] args) {
        MyThread myThread = new MyThread();
        myThread.start();

        Runnable runnable = new MyRunnableInterface();
        Thread runnableThread = new Thread(runnable);
        runnableThread.start();

        Thread lambdaRunnableThread = new Thread(() -> System.out.println("This is lambda interface...."));
        lambdaRunnableThread.start();
    }
}
