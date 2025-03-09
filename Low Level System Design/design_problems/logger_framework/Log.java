package design_problems.logger_framework;

import java.time.LocalDateTime;

public class Log {
    private final LogLevel logLevel;
    private final String message;
    private final LocalDateTime timestamp;

    public Log(LogLevel logLevel, String message, LocalDateTime timestamp) {
        this.logLevel = logLevel;
        this.message = message;
        this.timestamp = timestamp;
    }

    public LogLevel getLogLevel() {
        return logLevel;
    }

    public String getMessage() {
        return message;
    }

    public LocalDateTime getTimestamp() {
        return timestamp;
    }
}
