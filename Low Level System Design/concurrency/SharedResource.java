package concurrency;

public class SharedResource {
    int count = 0;

    void increment() {
        for (int i = 0; i < 10000; i++) {
            count++;
        }
    }

    public static void main(String[] args) {
        SharedResource sharedResource = new SharedResource();
        Runnable sharedRunnable = sharedResource::increment;
        Thread thread1 = new Thread(sharedRunnable, "Thread-1");
        Thread thread2 = new Thread(sharedRunnable, "Thread-2");
        Thread thread3 = new Thread(sharedRunnable, "Thread-3");
        Thread thread4 = new Thread(sharedRunnable, "Thread-4");
        Thread thread5 = new Thread(sharedRunnable, "Thread-5");
        thread1.start();
        thread2.start();
        thread3.start();
        thread4.start();
        thread5.start();
        try {
            thread1.join();
            thread2.join();
            thread3.join();
            thread4.join();
            thread5.join();
        } catch (InterruptedException e) {
            System.out.println(e.getMessage());
        }
        System.out.println(sharedResource.count);
    }
}