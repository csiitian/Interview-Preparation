package design_patterns.behavioral.chain_of_responsibility;

public class MainApplication {
    public static void main(String[] args) {
        ILogger logger = new DebugLogger(
                new InfoLogger(
                        new WarnLogger(
                                new ErrorLogger(null)
                        )
                )
        );
        logger.log(LogLevel.ERROR, "This is error message");
    }
}
