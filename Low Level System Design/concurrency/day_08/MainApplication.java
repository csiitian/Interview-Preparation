package concurrency.day_08;

public class MainApplication {
    public static void main(String[] args) {
        SharedPrinterUsingLock sharedPrinterUsingLock = new SharedPrinterUsingLock();
        Thread evenThread2 = new Thread(sharedPrinterUsingLock::printEven);
        evenThread2.setName("Even Thread");
        Thread oddThread2 = new Thread(sharedPrinterUsingLock::printOdd);
        oddThread2.setName("Odd Thread");
        evenThread2.start();
        oddThread2.start();
        try {
            evenThread2.join();
            oddThread2.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
