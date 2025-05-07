package design_problems.notification_system;

import design_problems.notification_system.channel.AndroidPushNotificationChannel;
import design_problems.notification_system.domain.User;
import design_problems.notification_system.domain.UserNotification;

import java.time.LocalDateTime;
import java.util.List;

public class MainApplication {
    public static void main(String[] args) throws InterruptedException {
        NotificationService notificationService = new NotificationService();
        AndroidPushNotificationChannel androidPushNotificationChannel = new AndroidPushNotificationChannel();
        User john = new User("1", "John Doe", "john.doe@gmail.com");
        john.subscribeToChannel(androidPushNotificationChannel);
        User bob = new User("2", "Bob Marlie", "bob.marlie@gmail.com");
        bob.subscribeToChannel(androidPushNotificationChannel);

        notificationService.pushNotification(new UserNotification("1", "This is simple", "Demo Summary", LocalDateTime.now(), john, List.of(bob)));
    }
}
