package concurrency;

public class MainApplication {

    public static void main(String[] args) {
        MyThread myThread = new MyThread();
        myThread.start();

        Runnable runnable = new MyRunnableInterface();
        runnable.run();

        Runnable lambdaRunnable = () -> System.out.println("This is lambda interface....");
        lambdaRunnable.run();

//        SharedResource sharedResource = new SharedResource();
//        SharedResourceWithSynchronization sharedResource = new SharedResourceWithSynchronization();
//        SharedResourceWithLock sharedResource = new SharedResourceWithLock();
//        System.out.println(sharedResource.count);
//        Runnable sharedRunnable = sharedResource::increment;
//        Thread thread1 = new Thread(sharedRunnable, "Thread-1");
//        Thread thread2 = new Thread(sharedRunnable, "Thread-2");
//        Thread thread3 = new Thread(sharedRunnable, "Thread-3");
//        Thread thread4 = new Thread(sharedRunnable, "Thread-4");
//        Thread thread5 = new Thread(sharedRunnable, "Thread-5");
//        thread1.start();
//        thread2.start();
//        thread3.start();
//        thread4.start();
//        thread5.start();
//        try {
//            thread1.join();
//            thread2.join();
//            thread3.join();
//            thread4.join();
//            thread5.join();
//        } catch (InterruptedException e) {
//            System.out.println(e.getMessage());
//        }
//        System.out.println(sharedResource.count);
    }
}
