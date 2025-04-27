package concurrency.day_10;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;

public class MessageProcessor {
    BlockingQueue<Message> queue;

    MessageProcessor() {
        queue = new LinkedBlockingQueue<>(100);
        ExecutorService executorService = Executors.newFixedThreadPool(10);
        for (int i = 0; i < 10; i++) {
            executorService.submit(this::consume);
        }
    }

    void produce(Message message) {
        try {
            queue.put(message);
            System.out.println(message + "published");
        } catch (InterruptedException e) {
            System.out.println(message + " not published");
        }
    }

    void consume() {
        while (true) {
            try {
                System.out.println(queue.take() + "consumed");
            } catch (InterruptedException e) {
                System.out.println("Error while consuming message");
            }
            try {
                Thread.sleep(10000);
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }
    }
}
