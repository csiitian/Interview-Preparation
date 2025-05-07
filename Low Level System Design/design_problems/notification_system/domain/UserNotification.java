package design_problems.notification_system.domain;

import java.time.LocalDateTime;
import java.util.List;

public class UserNotification extends Notification {
    User sender;

    public UserNotification(String id, String title, String summary, LocalDateTime createdAt, User sender, List<User> recipients) {
        super(id, title, summary, createdAt, recipients);
        this.sender = sender;
    }

    @Override
    public String toString() {
        return "User Notification: " + super.toString();
    }
}
