package design_patterns.Behavioral.Observer;

public interface IFObservable<T> {

  void add(IFObserver observer);

  void remove(IFObserver observer);

  void notifyObservers();

  void setData(T data);

  T getData();
}
