package concurrency.day_10;

public class Message {
    String message;
    Long timestamp;

    public Message(String message) {
        this.message = message;
        this.timestamp = System.currentTimeMillis();
    }

    @Override
    public String toString() {
        return "Message{" +
                "message='" + message + '\'' +
                ", timestamp=" + timestamp +
                '}';
    }
}
