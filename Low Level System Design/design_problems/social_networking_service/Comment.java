package design_problems.social_networking_service;

import java.time.LocalDateTime;

public class Comment {
    String commentId;
    String userId;
    String postId;
    String content;
    LocalDateTime createdAt;
    LocalDateTime updatedAt;
}
