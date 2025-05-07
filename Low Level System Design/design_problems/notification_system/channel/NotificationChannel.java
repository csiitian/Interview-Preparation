package design_problems.notification_system.channel;

import design_problems.notification_system.domain.Notification;
import design_problems.notification_system.domain.User;

public interface NotificationChannel {
    void sendNotification(User user, Notification notification);
}
