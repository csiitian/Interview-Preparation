package concurrency.day_03;

public class MainApplication {
    public static void main(String[] args) {
        SharedResource sharedResource = new SharedResource();
        Thread thread1 = new Thread(sharedResource::increment);
        Thread thread2 = new Thread(sharedResource::increment);
        thread1.start();
        thread2.start();
        try {
            thread1.join();
            thread2.join();
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        System.out.println(sharedResource.count);


        SharedResourceWithSynchronization sharedResourceWithSynchronization = new SharedResourceWithSynchronization();
        long startTime = System.currentTimeMillis();
        Thread thread3 = new Thread(sharedResourceWithSynchronization::increment);
        Thread thread4 = new Thread(sharedResourceWithSynchronization::increment);
        thread3.start();
        thread4.start();
        try {
            thread3.join();
            thread4.join();
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        long endTime = System.currentTimeMillis();
        System.out.println(sharedResourceWithSynchronization.count + " with synchronization time: " + (endTime - startTime));

        SharedResourceWithAtomicInteger sharedResourceWithAtomicInteger = new SharedResourceWithAtomicInteger();
        startTime = System.currentTimeMillis();
        Thread thread5 = new Thread(sharedResourceWithAtomicInteger::increment);
        Thread thread6 = new Thread(sharedResourceWithAtomicInteger::increment);
        thread5.start();
        thread6.start();
        try {
            thread5.join();
            thread6.join();
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        endTime = System.currentTimeMillis();
        System.out.println(sharedResourceWithAtomicInteger.count + " with atomic integer time: " + (endTime - startTime));
    }
}
