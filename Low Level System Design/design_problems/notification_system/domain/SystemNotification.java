package design_problems.notification_system.domain;

import java.time.LocalDateTime;
import java.util.List;

public class SystemNotification extends Notification {
    public SystemNotification(String id, String title, String summary, LocalDateTime createdAt, List<User> recipients) {
        super(id, title, summary, createdAt, recipients);
    }

    @Override
    public String toString() {
        return "User Notification: " + super.toString();
    }
}
