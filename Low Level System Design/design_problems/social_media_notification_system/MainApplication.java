package design_problems.social_media_notification_system;

import design_problems.social_media_notification_system.domain.Notification;
import design_problems.social_media_notification_system.domain.Type;
import design_problems.social_media_notification_system.domain.User;
import design_problems.social_media_notification_system.strategy.NotificationListStrategy;
import design_problems.social_media_notification_system.strategy.UnReadFirstNotificationListStrategy;

import java.util.List;

public class MainApplication {
    public static void main(String[] args) {
        NotificationListStrategy notificationListStrategy = new UnReadFirstNotificationListStrategy();
        INotificationsService notificationsService = new NotificationServiceImpl(notificationListStrategy);
        User userV = new User("vishal");
        User userS = new User("satish");
        notificationsService.registerUser(userV);
        notificationsService.registerUser(userS);

        Notification notificationV = notificationsService.createNotification(
                List.of(userV.getUserId()),
                new Notification("Satish has followed you.", Type.FOLLOW)
        );

        Notification notificationS = notificationsService.createNotification(
                List.of(userS.getUserId()),
                new Notification("Vishal has liked your post.", Type.LIKE)
        );

        List<Notification> notifications = notificationsService.getNotifications(userV.getUserId());
        System.out.println(notifications);

        notificationsService.markNotificationAsRead(userV.getUserId(), notificationV);
        notifications = notificationsService.getNotifications(userV.getUserId());
        System.out.println(notifications);
    }
}
