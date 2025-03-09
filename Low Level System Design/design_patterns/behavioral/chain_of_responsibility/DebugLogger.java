package design_patterns.behavioral.chain_of_responsibility;

public class DebugLogger extends ILogger {
    public DebugLogger(ILogger nextLogger) {
        super(nextLogger);
    }

    @Override
    void log(LogLevel logLevel, String message) {
        if (logLevel.ordinal() == LogLevel.DEBUG.ordinal()) {
            System.out.println("Debug: " + message);
            return;
        }
        if (nextLogger != null) {
            nextLogger.log(logLevel, message);
        }
    }
}
