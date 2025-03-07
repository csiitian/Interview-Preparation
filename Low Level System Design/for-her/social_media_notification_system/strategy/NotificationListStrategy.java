package strategy;

import domain.Notification;

import java.util.List;

public interface NotificationListStrategy {
    List<Notification> sortNotifications(List<Notification> notifications);
}
