package design_patterns.behavioral.observer;

public interface IFObservable<T> {

  void add(IFObserver observer);

  void remove(IFObserver observer);

  void notifyObservers();

  void setData(T data);

  T getData();
}
