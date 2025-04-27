package concurrency.day_07;

public class MainApplication {
    public static void main(String[] args) {
        MessageProcessor messageProcessor = new MessageProcessor();
        Thread prodcuerThread = new Thread(() -> {
            try {
                while (true) {
                    messageProcessor.produce(String.valueOf(System.currentTimeMillis()));
                    Thread.sleep(1000); // producer sleep time
                }
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        });
        prodcuerThread.start();
        Thread consumerThread = new Thread(() -> {
           try {
               while (true) {
                   messageProcessor.consume();
                   Thread.sleep(100); // consumer sleep time
               }
           } catch (InterruptedException e) {
               e.printStackTrace();
           }
        });
        consumerThread.start();
    }
}

/*
Note: Change the producer and consumer sleep time and see the logs.
 */