package concurrency.day_03;

public class SharedResourceWithSynchronization {
    int count = 0;

    synchronized void increment() {
        for (int i = 0; i < 100000; i++) {
            count++;
        }
    }
}