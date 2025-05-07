package design_problems.notification_system.domain;

import java.time.LocalDateTime;
import java.util.List;

public abstract class Notification {
    private String id;
    private String title;
    private String summary;
    private LocalDateTime createdAt;
    private List<User> recipients;

    Notification(String id, String title, String summary, LocalDateTime createdAt, List<User> recipients) {
        this.id = id;
        this.title = title;
        this.summary = summary;
        this.createdAt = createdAt;
        this.recipients = recipients;
    }

    public List<User> getRecipients() {
        return recipients;
    }

    @Override
    public String toString() {
        return "Notification{" +
                "id='" + id + '\'' +
                ", title='" + title + '\'' +
                ", summary='" + summary + '\'' +
                ", createdAt=" + createdAt +
                ", recipients=" + recipients +
                '}';
    }
}
