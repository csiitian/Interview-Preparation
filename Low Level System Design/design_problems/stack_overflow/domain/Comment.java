package design_problems.stack_overflow.domain;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class Comment {
    String commentId;
    String content;
    User author;
    EntityType entityType;
    String entityId;
    Map<User, VoteType> votes;
    List<Reply> replies;
    LocalDateTime createdAt;
    LocalDateTime updateAt;

    public Comment(EntityType entityType, String entityId, String content) {
        this.entityType = entityType;
        this.entityId = entityId;
        this.content = content;
        this.votes = new HashMap<>();
        this.replies = new ArrayList<>();
        this.createdAt = LocalDateTime.now();
        this.updateAt = LocalDateTime.now();
    }

    public String getCommentId() {
        return commentId;
    }

    public void setCommentId(String commentId) {
        this.commentId = commentId;
    }

    public String getContent() {
        return content;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public User getAuthor() {
        return author;
    }

    public void setAuthor(User author) {
        this.author = author;
    }

    public EntityType getEntityType() {
        return entityType;
    }

    public void setEntityType(EntityType entityType) {
        this.entityType = entityType;
    }

    public String getEntityId() {
        return entityId;
    }

    public void setEntityId(String entityId) {
        this.entityId = entityId;
    }

    public Map<User, VoteType> getVotes() {
        return votes;
    }

    public void setVotes(Map<User, VoteType> votes) {
        this.votes = votes;
    }

    public List<Reply> getReplies() {
        return replies;
    }

    public void setReplies(List<Reply> replies) {
        this.replies = replies;
    }

    public LocalDateTime getUpdateAt() {
        return updateAt;
    }

    public void setUpdateAt(LocalDateTime updateAt) {
        this.updateAt = updateAt;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    @Override
    public String toString() {
        return "Comment{" +
                "commentId='" + commentId + '\'' +
                ",\n content='" + content + '\'' +
                ",\n author=" + author +
                ",\n entityType=" + entityType +
                ",\n entityId='" + entityId + '\'' +
                ",\n votes=" + votes +
                ",\n replies=" + replies +
                '}';
    }
}
