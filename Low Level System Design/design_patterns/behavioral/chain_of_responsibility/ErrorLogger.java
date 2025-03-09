package design_patterns.behavioral.chain_of_responsibility;

public class ErrorLogger extends ILogger {
    public ErrorLogger(ILogger nextLogger) {
        super(nextLogger);
    }

    @Override
    void log(LogLevel logLevel, String message) {
        if (logLevel.ordinal() <= LogLevel.ERROR.ordinal()) {
            System.err.println("Error: " + message);
            return;
        }
        if (nextLogger != null) {
            nextLogger.log(logLevel, message);
        }
    }
}
