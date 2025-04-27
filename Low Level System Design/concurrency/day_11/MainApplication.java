package concurrency.day_11;

public class MainApplication {
    public static void main(String[] args) {
        SemaphoreExample semaphoreExample = new SemaphoreExample(3);
        Thread[] threads = new Thread[10];
        for (int i = 0; i < threads.length; i++) {
            threads[i] = new Thread(semaphoreExample::execute);
            threads[i].start();
        }
    }
}
