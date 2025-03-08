package design_problems.social_media_notification_system;

import design_problems.social_media_notification_system.domain.Notification;
import design_problems.social_media_notification_system.domain.User;
import design_problems.social_media_notification_system.strategy.NotificationListStrategy;

import java.util.List;

public interface INotificationsService {
    void registerUser(User user);
    void loginUser(User user);
    void setNotificationListStrategy(NotificationListStrategy strategy);
    List<Notification> getNotifications(String userId);
    Notification createNotification(List<String> userIds, Notification notification);
    void markNotificationAsRead(String userId, Notification notification);
    void markAllNotificationsAsRead(String userId);
}
