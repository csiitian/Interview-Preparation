package concurrency.day_01;

public class MainApplication {
    public static void main(String[] args) {
        MyThread thread = new MyThread();
        thread.start();

        thread.run(); // not creating new thread, executing from main thread
    }
}
