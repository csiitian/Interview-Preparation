package design_problems.stack_overflow.domain;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;
import java.util.UUID;

public class Answer {
    String answerId;
    String content;
    User author;
    String questionId;
    List<Comment> comments;
    Map<User, VoteType> votes;
    boolean isAccepted;
    LocalDateTime createdAt;
    LocalDateTime updatedAt;

    public Answer(String questionId, String content) {
        this.answerId = "A" + UUID.randomUUID();
        this.questionId = questionId;
        this.content = content;
    }

    public String getAnswerId() {
        return answerId;
    }

    public void setAnswerId(String answerId) {
        this.answerId = answerId;
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

    public String getQuestionId() {
        return questionId;
    }

    public void setQuestionId(String questionId) {
        this.questionId = questionId;
    }

    public List<Comment> getComments() {
        return comments;
    }

    public void setComments(List<Comment> comments) {
        this.comments = comments;
    }

    public Map<User, VoteType> getVotes() {
        return votes;
    }

    public void setVotes(Map<User, VoteType> votes) {
        this.votes = votes;
    }

    public boolean isAccepted() {
        return isAccepted;
    }

    public void setAccepted(boolean accepted) {
        isAccepted = accepted;
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
        return "Answer{" +
                "answerId='" + answerId + '\'' +
                ",\n content='" + content + '\'' +
                ",\n author=" + author +
                ",\n questionId='" + questionId + '\'' +
                ",\n comments=" + comments +
                ",\n votes=" + votes +
                ",\n isAccepted=" + isAccepted +
                '}';
    }
}
