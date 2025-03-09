package design_problems.logger_framework;

public class ConsoleLogAppender implements LogAppender {
    @Override
    public void append(String message) {
        System.out.println(message);
    }
}
