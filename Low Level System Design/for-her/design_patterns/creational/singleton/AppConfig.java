package design_patterns.creational.singleton;

public class AppConfig {

    private static volatile AppConfig instance;

    private AppConfig() {}

    public static AppConfig getInstance() {
        if (instance == null) {
            synchronized (AppConfig.class) {
                if (instance == null) {
                    instance = new AppConfig();
                }
            }
        }
        return instance;
    }

    public void showMessage() {
        System.out.println("Hi, from App Config.");
    }
}
