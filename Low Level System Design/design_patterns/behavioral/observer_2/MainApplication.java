package design_patterns.behavioral.observer_2;

public class MainApplication {
    public static void main(String[] args) {
        ISubject subject = new Subject();
        IObserver temperatureObserver = new TemperatureObserver(subject);
        IObserver humidityObserver = new HumidityObserver(subject);

        subject.addObserver(temperatureObserver);
        subject.addObserver(humidityObserver);

        subject.updateTemperature(37.7d);

        subject.removeObserver(temperatureObserver);

        subject.updateHumidity(6.55d);
    }
}
