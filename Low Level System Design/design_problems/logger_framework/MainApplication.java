package design_problems.logger_framework;

import java.util.List;

import static design_problems.logger_framework.LogLevel.*;

public class MainApplication {
    public static void main(String[] args) throws InterruptedException {
        ConsoleLogAppender consoleLogAppender = new ConsoleLogAppender();
        FileLogAppender fileLogAppender1 = new FileLogAppender("./design_problems/logger_framework/demo1.txt");
        FileLogAppender fileLogAppender2 = new FileLogAppender("./design_problems/logger_framework/demo2.txt");
        LoggerConfig loggerConfig = new LoggerConfig(INFO, List.of(consoleLogAppender, fileLogAppender1, fileLogAppender2));
        Logger logger = new Logger(loggerConfig);
        logger.log(INFO, "This is info message");
        Thread.sleep(2000);
        logger.log(DEBUG, "This is debug message");
        Thread.sleep(2000);
        logger.log(ERROR, "This is error message");
    }
}
