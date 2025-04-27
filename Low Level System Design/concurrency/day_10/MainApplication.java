package concurrency.day_10;

public class MainApplication {
    public static void main(String[] args) {
        MessageProcessor messageProcessor = new MessageProcessor();
        for (int i = 0; i < 100; i++) {
            messageProcessor.produce(new Message(String.valueOf(i)));
        }
    }
}
