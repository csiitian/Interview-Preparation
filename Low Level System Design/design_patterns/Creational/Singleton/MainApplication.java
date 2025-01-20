package design_patterns.Creational.Singleton;

public class MainApplication {

  public static void main(String[] args) {
    SingleInstance singleton = SingleInstance.getInstance();
    singleton.print();
  }
}
