package design_problems.social_media_notification_system.strategy;

import design_problems.social_media_notification_system.domain.Notification;

import java.util.List;

public interface NotificationListStrategy {
    List<Notification> sortNotifications(List<Notification> notifications);
}
