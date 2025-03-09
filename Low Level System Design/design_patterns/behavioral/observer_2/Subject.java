package design_patterns.behavioral.observer_2;

import java.util.ArrayList;
import java.util.List;

public class Subject implements ISubject {
    List<IObserver> observers;
    double temperature;
    double humidity;

    Subject() {
        observers = new ArrayList<>();
    }

    @Override
    public void addObserver(IObserver observer) {
        observers.add(observer);
    }

    @Override
    public void removeObserver(IObserver observer) {
        observers.remove(observer);
    }

    @Override
    public void notifyObservers() {
        for (IObserver observer: observers) {
            observer.update();
        }
    }

    @Override
    public double getTemperature() {
        return temperature;
    }

    @Override
    public double getHumidity() {
        return humidity;
    }

    @Override
    public void updateTemperature(double temperature) {
        this.temperature = temperature;
        notifyObservers();
    }

    @Override
    public void updateHumidity(double humidity) {
        this.humidity = humidity;
        notifyObservers();
    }
}
