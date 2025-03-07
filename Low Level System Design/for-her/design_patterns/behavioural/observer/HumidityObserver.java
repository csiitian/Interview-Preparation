package design_patterns.behavioural.observer;

public class HumidityObserver implements IObserver {
    ISubject subject;

    HumidityObserver(ISubject subject) {
        this.subject = subject;
    }

    @Override
    public void update() {
        System.out.println("Humidity: " + subject.getHumidity());
    }
}
