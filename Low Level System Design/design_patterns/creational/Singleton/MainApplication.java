package design_patterns.creational.Singleton;

public class MainApplication {

  public static void main(String[] args) {
    SingleInstance singleton = SingleInstance.getInstance();
    singleton.print();
  }
}
