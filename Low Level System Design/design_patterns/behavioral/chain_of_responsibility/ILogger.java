package design_patterns.behavioral.chain_of_responsibility;

public abstract class ILogger {
    ILogger nextLogger;

    ILogger(ILogger nextLogger) {
        this.nextLogger = nextLogger;
    }

    abstract void log(LogLevel logLevel, String message);
}
