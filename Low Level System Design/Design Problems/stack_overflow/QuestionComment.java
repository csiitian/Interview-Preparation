package system_design.stack_overflow;

import java.time.LocalDateTime;
import java.util.Map;

public class QuestionComment {
    String commentId;
    String description;
    User author;
    Question question;
    Map<User, VoteType> votes;
    LocalDateTime createdAt;
    LocalDateTime updateAt;
}
