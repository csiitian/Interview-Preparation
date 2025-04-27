package concurrency.day_07;

import java.util.LinkedList;
import java.util.Queue;

public class MessageProcessor {
    Queue<String> buffer;
    int MAX_BUFFER_SIZE = 5;

    MessageProcessor() {
        buffer = new LinkedList<>();
    }

    public synchronized void produce(String message) {
        try {
            while (buffer.size() == MAX_BUFFER_SIZE) {
                System.out.println("Producer waiting for message to produce");
                wait();
            }
            buffer.offer(message);
            System.out.println(Thread.currentThread().getName() + " produced " + message);
            notifyAll();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    public synchronized void consume() {
        try {
            while (buffer.isEmpty()) {
                System.out.println("Consumer waiting for message to consume");
                wait();
            }
            String message = buffer.poll();
            System.out.println(Thread.currentThread().getName() + " consumed " + message);
            notifyAll();
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }
}
