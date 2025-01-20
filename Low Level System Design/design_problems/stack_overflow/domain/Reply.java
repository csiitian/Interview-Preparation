package design_problems.stack_overflow.domain;

import java.time.LocalDateTime;
import java.util.HashMap;
import java.util.Map;

public class Reply {
    String replyId;
    String commentId;
    String content;
    User author;
    Map<User, VoteType> votes;
    LocalDateTime createdAt;
    LocalDateTime updatedAt;

    public Reply(String commentId, String content) {
        this.commentId = commentId;
        this.content = content;
        this.votes = new HashMap<>();
        this.createdAt = LocalDateTime.now();
        this.updatedAt = LocalDateTime.now();
    }

    public String getReplyId() {
        return replyId;
    }

    public void setReplyId(String replyId) {
        this.replyId = replyId;
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

    public Map<User, VoteType> getVotes() {
        return votes;
    }

    public void setVotes(Map<User, VoteType> votes) {
        this.votes = votes;
    }

    public LocalDateTime getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(LocalDateTime createdAt) {
        this.createdAt = createdAt;
    }

    public LocalDateTime getUpdatedAt() {
        return updatedAt;
    }

    public void setUpdatedAt(LocalDateTime updatedAt) {
        this.updatedAt = updatedAt;
    }

    @Override
    public String toString() {
        return "Reply{" +
                "replyId='" + replyId + '\'' +
                ",\n commentId='" + commentId + '\'' +
                ",\n content='" + content + '\'' +
                ",\n author=" + author +
                ",\n votes=" + votes +
                '}';
    }
}
