package design_problems.push_notification_system.channel;

import design_problems.social_media_notification_system.domain.Notification;
import design_problems.social_media_notification_system.domain.User;

public interface NotificationChannel {
    void sendNotification(User user, Notification notification);
}
