package design_patterns.behavioral.observer;

public class ObserverImpl implements IFObserver {

  String name;
  IFObservable<String> observable;

  public ObserverImpl(String name, IFObservable<String> observable) {
    this.name = name;
    this.observable = observable;
  }

  @Override
  public void update() {
    System.out.println(name + " is notified with latest data: " + observable.getData());
  }

  @Override
  public String getName() {
    return name;
  }
}
