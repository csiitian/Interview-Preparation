package design_patterns.behavioral.observer;

public class MainApplication {

  public static void main(String[] args) {

    IFObservable<String> observable = new StringObservableImpl();
    IFObserver observer1 = new ObserverImpl("alpha", observable);
    IFObserver observer2 = new ObserverImpl("beta", observable);
    IFObserver observer3 = new ObserverImpl("gamma", observable);
    IFObserver observer4 = new ObserverImpl("theta", observable);

    observable.add(observer1);
    observable.add(observer2);
    observable.add(observer3);
    observable.add(observer4);

    for (int i = 0; i < 5; i++) {
      observable.setData(String.valueOf(i));
      try {
        Thread.sleep(2000);
      } catch (InterruptedException e) {
        System.out.println(e.getMessage());
      }
    }

    Runtime.getRuntime().addShutdownHook(new Thread(() -> {
      System.out.println("Shutdown Hook is running !");
      observable.remove(observer1);
      observable.remove(observer2);
      observable.remove(observer3);
      observable.remove(observer4);
    }));
  }
}
