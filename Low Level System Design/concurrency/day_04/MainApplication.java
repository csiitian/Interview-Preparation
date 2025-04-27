package concurrency.day_04;

public class MainApplication {
    public static void main(String[] args) throws InterruptedException {
        SharedPrinter sharedPrinter = new SharedPrinter();
        Thread evenThread = new Thread(sharedPrinter::printEven);
        evenThread.setName("Even Thread");
        Thread oddThread = new Thread(sharedPrinter::printOdd);
        oddThread.setName("Odd Thread");
        evenThread.start();
        oddThread.start();
        try {
            evenThread.join();
            oddThread.join();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
