package system_design.stack_overflow;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

public class Question {
    String questionId;
    String title;
    String description;
    User author;
    Map<User, VoteType> votes;
    boolean isAnswered;
    List<QuestionTags> tags;
    List<Answer> answers;
    List<QuestionComment> comments;
    LocalDateTime createdAt;
    LocalDateTime updateAt;
}
