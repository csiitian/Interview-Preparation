package design_problems.logger_framework;

import java.util.List;

public class LoggerConfig {
    private LogLevel logLevel;
    List<LogAppender> logAppenderList;

    public LoggerConfig(LogLevel logLevel, List<LogAppender> logAppenderList) {
        this.logLevel = logLevel;
        this.logAppenderList = logAppenderList;
    }

    public LogLevel getLogLevel() {
        return logLevel;
    }

    public void setLogLevel(LogLevel logLevel) {
        this.logLevel = logLevel;
    }

    public void addLogAppender(LogAppender logAppender) {
        logAppenderList.add(logAppender);
    }
}
