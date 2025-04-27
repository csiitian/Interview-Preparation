package concurrency.day_3;

import java.util.concurrent.atomic.AtomicInteger;

public class SharedResourceWithAtomicInteger {
    AtomicInteger count = new AtomicInteger(0);

    void increment() {
        for (int i = 0; i < 100000; i++) {
            count.incrementAndGet();
        }
    }
}