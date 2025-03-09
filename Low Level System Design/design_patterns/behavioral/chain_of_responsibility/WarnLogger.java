package design_patterns.behavioral.chain_of_responsibility;

public class WarnLogger extends ILogger {
    public WarnLogger(ILogger nextLogger) {
        super(nextLogger);
    }

    @Override
    void log(LogLevel logLevel, String message) {
        if (logLevel.ordinal() <= LogLevel.WARN.ordinal()) {
            System.out.println("Warning: " + message);
            return;
        }
        if (nextLogger != null) {
            nextLogger.log(logLevel, message);
        }
    }
}
