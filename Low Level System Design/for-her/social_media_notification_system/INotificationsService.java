import domain.Notification;
import domain.User;
import strategy.NotificationListStrategy;

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
