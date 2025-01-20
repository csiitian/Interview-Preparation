package design_problems.stack_overflow;

import design_problems.stack_overflow.domain.*;

public class MainApplication {
    public static void main(String[] args) {
        System.out.println("Hello, LLD for StackOverflow");

        StackOverflowService stackOverflowService = new StackOverflowService();

        User user = new User("Vishal", "vishalsinghgk2018@gmail.com");
        User user2 = new User("Vikas", "vishalsinghgk2021@gmail.com");
        stackOverflowService.addUser(user);
        stackOverflowService.addUser(user2);

        Question question = new Question("How to design LLD for StackOverflow?", "This is very simple design , we are" +
                " going to show.");
        stackOverflowService.addQuestion(user, question);

        Answer answer = new Answer(question.getQuestionId(), "This is how you can design LLD for StackOverflow");
        stackOverflowService.addAnswer(user2, answer);

        Comment comment = new Comment(EntityType.QUESTION, question.getQuestionId(), "This is a good question");
        stackOverflowService.addComment(user, comment);

        Reply reply = new Reply(comment.getCommentId(), "Yeah !! I agree with you");
        stackOverflowService.addReply(user2, reply);

        stackOverflowService.getQuestion(question.getQuestionId());
    }
}
