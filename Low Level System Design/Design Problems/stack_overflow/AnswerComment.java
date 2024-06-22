package system_design.stack_overflow;

import java.time.LocalDateTime;
import java.util.Map;

public class AnswerComment {
    String commentId;
    String description;
    User author;
    Answer answer;
    Map<User, VoteType> votes;
    LocalDateTime createdAt;
    LocalDateTime updateAt;
}
