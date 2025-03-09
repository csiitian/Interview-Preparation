package design_patterns.creational.Singleton;

public class SingleInstance {

  private static volatile SingleInstance instance;

  // private constructor
  private SingleInstance() {
  }

  public static SingleInstance getInstance() {
    if (instance == null) {
      synchronized (SingleInstance.class) {
        if (instance == null) {
          instance = new SingleInstance();
        }
      }
    }
    return instance;
  }

  public void print() {
    System.out.println("Single instance");
  }
}
