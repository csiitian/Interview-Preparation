package design_patterns.Behavioral.Observer;

public class ObserverImpl implements IFObserver {

  String name;
  IFObservable<String> observable;

  public ObserverImpl(String name, IFObservable<String> observable) {
    this.name = name;
    this.observable = observable;
  }

  @Override
  public void update() {
    System.out.println("I am notified with latest data: " + observable.getData());
  }

  @Override
  public String getName() {
    return name;
  }
}
