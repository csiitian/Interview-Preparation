package design_problems.social_media_notification_system;

import design_problems.social_media_notification_system.domain.Notification;
import design_problems.social_media_notification_system.domain.Status;
import design_problems.social_media_notification_system.domain.User;
import design_problems.social_media_notification_system.strategy.NotificationListStrategy;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class NotificationServiceImpl implements INotificationsService {
    NotificationListStrategy notificationListStrategy;
    Map<String, User> users;

    NotificationServiceImpl(NotificationListStrategy notificationListStrategy) {
        this.notificationListStrategy = notificationListStrategy;
        this.users = new HashMap<>();
    }

    @Override
    public void registerUser(User user) {
        this.users.put(user.getUserId(), user);
    }

    @Override
    public void loginUser(User user) {
        if (!this.users.containsKey(user.getUserId())) {
            throw new RuntimeException("User is not registered.");
        }
    }

    @Override
    public void setNotificationListStrategy(NotificationListStrategy strategy) {
        this.notificationListStrategy = strategy;
    }

    @Override
    public List<Notification> getNotifications(String userId) {
        User user = users.get(userId);
        if (user == null) {
            throw new RuntimeException("No user found.");
        }
        List<Notification> notifications = user.getNotificationList();
        return notificationListStrategy.sortNotifications(notifications);
    }

    @Override
    public Notification createNotification(List<String> userIds, Notification notification) {
        for (String userId: userIds) {
            User user = users.get(userId);
            if (user != null) {
                user.getNotificationList().add(notification);
            }
        }
        return notification;
    }

    @Override
    public void markNotificationAsRead(String userId, Notification notification) {
        User user = users.get(userId);
        if (user == null) return;
        for (Notification notificationItr: user.getNotificationList()) {
            if (notificationItr.getId() == notification.getId()) {
                notification.setStatus(Status.READ);
                return;
            }
        }
    }

    @Override
    public void markAllNotificationsAsRead(String userId) {
        User user = users.get(userId);
        if (user == null) return;
        for (Notification notification: user.getNotificationList()) {
            if (notification.getStatus() == Status.UNREAD) {
                notification.setStatus(Status.READ);
            }
        }
    }
}
