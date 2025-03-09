package design_problems.logger_framework;

import java.time.LocalDateTime;

public class Logger {
    private final LoggerConfig config;
    private final LogQueue logQueue;

    public Logger(LoggerConfig config) {
        this.config = config;
        this.logQueue = new LogQueue();
        startAsyncLogger();
    }

    public void log(LogLevel logLevel, String message) {
        if (config.getLogLevel().ordinal() <= logLevel.ordinal()) {
            try {
                logQueue.addLog(new Log(logLevel, message, LocalDateTime.now()));
            } catch (InterruptedException e) {
                System.out.println(e.getMessage());
            }
        }
    }

    private void startAsyncLogger() {
        new Thread(() -> {
            while(true) {
                try {
                    Log log = logQueue.pollLog();
                    for (LogAppender logAppender: config.logAppenderList) {
                        logAppender.append(LogFormatter.formatLog(log));
                    }
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                }
            }
        }).start();
    }
}
