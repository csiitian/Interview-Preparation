package design_problems.logger_framework;

public class LogFormatter {
    static String formatLog(Log log) {
        return log.getTimestamp() + " - " + log.getLogLevel() + ": " + log.getMessage();
    }
}
