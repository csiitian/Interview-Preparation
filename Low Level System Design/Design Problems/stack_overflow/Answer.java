package system_design.stack_overflow;

import java.time.LocalDateTime;
import java.util.List;
import java.util.Map;

public class Answer {
    String answerId;
    String description;
    User author;
    Question question;
    List<AnswerComment> comments;
    Map<User, VoteType> votes;
    boolean isAccepted;
    LocalDateTime createdAt;
    LocalDateTime updatedAt;
}
