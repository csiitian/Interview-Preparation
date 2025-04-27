package concurrency.day_02;

public class MainApplication {

    public static void main(String[] args) {
        Runnable runnable = new MyRunnableInterface();
        Thread runnableThread = new Thread(runnable);
        runnableThread.start();

        runnableThread.run(); // not creating new thread, executing from main thread
    }
}
