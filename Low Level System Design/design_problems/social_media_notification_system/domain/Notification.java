package design_problems.social_media_notification_system.domain;

import java.time.LocalDateTime;
import java.util.UUID;

public class Notification {
    private String notificationId;
    private String message;
    private Type type;
    private LocalDateTime timestamp;
    private Status status;

    public Notification(String message, Type type) {
        this.notificationId = UUID.randomUUID().toString();
        this.message = message;
        this.type = type;
        this.timestamp = LocalDateTime.now();
        this.status = Status.UNREAD;
    }

    public String getNotificationId() {
        return notificationId;
    }

    public String getMessage() {
        return message;
    }

    public void setMessage(String message) {
        this.message = message;
    }

    public Type getType() {
        return type;
    }

    public void setType(Type type) {
        this.type = type;
    }

    public LocalDateTime getTimestamp() {
        return timestamp;
    }

    public void setTimestamp(LocalDateTime timestamp) {
        this.timestamp = timestamp;
    }

    public Status getStatus() {
        return status;
    }

    public void setStatus(Status status) {
        this.status = status;
    }

    @Override
    public String toString() {
        return "Notification{" +
                "message='" + message + '\'' +
                ", type=" + type +
                ", status=" + status +
                '}';
    }
}
