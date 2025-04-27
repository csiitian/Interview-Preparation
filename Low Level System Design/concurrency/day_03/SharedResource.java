package concurrency.day_03;

public class SharedResource {
    int count = 0;

    void increment() {
        for (int i = 0; i < 10000; i++) {
            count++;
        }
    }
}