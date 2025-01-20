package concurrency;

public class SharedResourceWithSynchronization {
    int count = 0;

    synchronized void increment() {
        for (int i = 0; i < 1000000; i++) {
            count++;
//            System.out.println(Thread.currentThread().getName());
        }
    }

    public static void main(String[] args) {
        long startTimestamp = System.currentTimeMillis();
        SharedResourceWithSynchronization sharedResource = new SharedResourceWithSynchronization();
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
        long endTimestamp = System.currentTimeMillis();
        System.out.println(sharedResource.count + " " + ((endTimestamp - startTimestamp) / 1000d));
    }
}