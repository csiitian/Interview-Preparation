package design_problems.stack_overflow;

import design_problems.stack_overflow.domain.*;

import java.util.HashMap;
import java.util.Map;

public class StackOverflowService {
    Map<String, User> users;
    Map<String, Question> questions;
    Map<String, Answer> answers;
    Map<String, Comment> comments;
    Map<String, Reply> replies;

    StackOverflowService() {
        users = new HashMap<>();
        questions = new HashMap<>();
        answers = new HashMap<>();
        comments = new HashMap<>();
        replies = new HashMap<>();
    }

    public void addUser(User user) {
        users.put(user.getUserId(), user);
    }

    public void addQuestion(User user, Question question) {
        question.setAuthor(user);
        user.getQuestions().add(question);
        questions.put(question.getQuestionId(), question);
    }

    public void addAnswer(User user, Answer answer) {
        answer.setAuthor(user);
        Question question = questions.get(answer.getQuestionId());
        if (question != null) question.getAnswers().add(answer);
        answers.put(answer.getAnswerId(), answer);
    }

    public void addComment(User user, Comment comment) {
        comment.setAuthor(user);
        if (comment.getEntityType() == null) return;
        if (comment.getEntityType() == EntityType.QUESTION) {
            Question question = questions.get(comment.getEntityId());
            if (question != null) question.getComments().add(comment);
        } else if (comment.getEntityType() == EntityType.ANSWER) {
            Answer answer = answers.get(comment.getEntityId());
            if (answer != null) answer.getComments().add(comment);
        }
        comments.put(comment.getCommentId(), comment);
    }

    public void addReply(User user, Reply reply) {
        reply.setAuthor(user);
        Comment comment = comments.get(reply.getCommentId());
        if (comment != null) comment.getReplies().add(reply);
        replies.put(reply.getReplyId(), reply);
    }

    public void getQuestion(String questionId) {
        Question question = questions.get(questionId);
        if (question != null) {
            System.out.println(question);
        }
    }
}
