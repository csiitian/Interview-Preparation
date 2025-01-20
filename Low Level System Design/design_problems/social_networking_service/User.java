package design_problems.social_networking_service;

import java.time.LocalDateTime;
import java.util.List;

public class User {
    String userId;
    String name;
    String email;
    Profile profile;
    boolean isPrivateAccount;
    List<User> friends;
    List<Post> posts;
    LocalDateTime createdAt;
    LocalDateTime updatedAt;
}
