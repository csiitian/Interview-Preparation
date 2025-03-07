package strategy;

import domain.Notification;
import domain.Status;

import java.util.Comparator;
import java.util.List;

public class UnReadFirstNotificationListStrategy implements NotificationListStrategy {
    @Override
    public List<Notification> sortNotifications(List<Notification> notifications) {
        return notifications.stream()
                .sorted(Comparator.comparing(notification -> notification.getStatus() == Status.UNREAD ? 0 : 1))
                .toList();
    }
}
