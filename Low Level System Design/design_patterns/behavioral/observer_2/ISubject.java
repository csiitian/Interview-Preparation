package design_patterns.behavioral.observer_2;

public interface ISubject {
    void addObserver(IObserver observer);
    void removeObserver(IObserver observer);
    void notifyObservers();
    double getTemperature();
    double getHumidity();
    void updateTemperature(double temperature);
    void updateHumidity(double humidity);
}
