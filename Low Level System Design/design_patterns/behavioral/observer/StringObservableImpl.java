package design_patterns.behavioral.observer;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class StringObservableImpl implements IFObservable<String> {

  List<IFObserver> observerList = new ArrayList<>();
  String data;

  @Override
  public void add(IFObserver observer) {
    observerList.add(observer);
    System.out.println(observer.getName() + " subscribed.");
  }

  @Override
  public void remove(IFObserver observer) {
    observerList.remove(observer);
    System.out.println(observer.getName() + " unsubscribed.");
  }

  @Override
  public void notifyObservers() {
    observerList.forEach(IFObserver::update);
  }

  @Override
  public String getData() {
    return this.data;
  }

  @Override
  public void setData(String data) {
    if (!Objects.equals(data, this.data)) {
      this.data = data;
      this.notifyObservers();
    }
  }
}
