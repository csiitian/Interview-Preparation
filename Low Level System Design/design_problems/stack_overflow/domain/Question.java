package design_problems.stack_overflow.domain;

import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.UUID;

public class Question {
    String questionId;
    String title;
    String description;
    User author;
    Map<User, VoteType> votes;
    boolean isAnswered;
    List<QuestionTags> tags;
    List<Answer> answers;
    List<Comment> comments;
    LocalDateTime createdAt;
    LocalDateTime updateAt;

    public Question(String title, String description) {
        this.questionId = "Q" + UUID.randomUUID();
        this.title = title;
        this.description = description;
        this.tags = new ArrayList<>();
        this.answers = new ArrayList<>();
        this.comments = new ArrayList<>();
        this.createdAt = LocalDateTime.now();
        this.updateAt = LocalDateTime.now();
    }

    public String getQuestionId() {
        return questionId;
    }

    public void setQuestionId(String questionId) {
        this.questionId = questionId;
    }

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
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

    public boolean isAnswered() {
        return isAnswered;
    }

    public void setAnswered(boolean answered) {
        isAnswered = answered;
    }

    public List<QuestionTags> getTags() {
        return tags;
    }

    public void setTags(List<QuestionTags> tags) {
        this.tags = tags;
    }

    public List<Answer> getAnswers() {
        return answers;
    }

    public void setAnswers(List<Answer> answers) {
        this.answers = answers;
    }

    public List<Comment> getComments() {
        return comments;
    }

    public void setComments(List<Comment> comments) {
        this.comments = comments;
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
        return "Question{" +
                "questionId='" + questionId + '\'' +
                ",\n title='" + title + '\'' +
                ",\n description='" + description + '\'' +
                ",\n author=" + author +
                ",\n votes=" + votes +
                ",\n isAnswered=" + isAnswered +
                ",\n tags=" + tags +
                ",\n answers=" + answers +
                ",\n comments=" + comments +
                '}';
    }
}
