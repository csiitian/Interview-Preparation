package design_patterns.behavioral.chain_of_responsibility;

public class InfoLogger extends ILogger {
    public InfoLogger(ILogger nextLogger) {
        super(nextLogger);
    }

    @Override
    void log(LogLevel logLevel, String message) {
        if (logLevel.ordinal() <= LogLevel.INFO.ordinal()) {
            System.out.println("Info: " + message);
            return;
        }
        if (nextLogger != null) {
            nextLogger.log(logLevel, message);
        }
    }
}
