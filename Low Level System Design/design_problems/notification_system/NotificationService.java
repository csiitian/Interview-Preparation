package design_problems.notification_system;

import design_problems.notification_system.channel.NotificationChannel;
import design_problems.notification_system.domain.Notification;
import design_problems.notification_system.domain.User;
import design_problems.notification_system.domain.UserNotification;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class NotificationService {
    private BlockingQueue<Notification> queue;

    public NotificationService() {
        queue = new LinkedBlockingQueue<>();
        new Thread(() -> {
            try {
                while(true) {
                    listen();
                }
            } catch (InterruptedException e) {
                throw new RuntimeException(e);
            }
        }).start();
    }

    public void pushNotification(Notification notification) throws InterruptedException {
        queue.put(notification);
    }

    private void listen() throws InterruptedException {
        Notification notification = queue.take();
        if (notification instanceof UserNotification userNotification) {
            for (User user: userNotification.getRecipients()) {
                for (NotificationChannel notificationChannel: user.getChannels()) {
                    notificationChannel.sendNotification(user, userNotification);
                }
            }
        }
    }
}
