package design_problems.notification_system.domain;

import design_problems.notification_system.channel.NotificationChannel;

import java.util.HashSet;
import java.util.Set;

public class User {
    private String id;
    private String name;
    private String email;
    private String password;
    private Set<NotificationChannel> channels;

    public User(String id, String name, String email) {
        this.id = id;
        this.name = name;
        this.email = email;
        this.channels = new HashSet<>();
    }

    public void subscribeToChannel(NotificationChannel channel) {
        channels.add(channel);
    }

    public Set<NotificationChannel> getChannels() {
        return channels;
    }

    @Override
    public String toString() {
        return "User{" +
                "id='" + id + '\'' +
                ", name='" + name + '\'' +
                ", email='" + email + '\'' +
                ", channels=" + channels +
                '}';
    }
}
