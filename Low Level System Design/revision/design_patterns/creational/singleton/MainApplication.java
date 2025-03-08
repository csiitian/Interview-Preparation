package design_patterns.creational.singleton;

public class MainApplication {
    public static void main(String[] args) {
        AppConfig appConfig = AppConfig.getInstance();
        appConfig.showMessage();
    }
}
