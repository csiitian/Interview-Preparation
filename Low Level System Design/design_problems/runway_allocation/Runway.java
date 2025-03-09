package design_problems.runway_allocation;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Runway {
    private final String id;
    private boolean isOccupied;
    private final Lock lock;

    Runway(String id) {
        this.id = id;
        isOccupied = false;
        lock = new ReentrantLock();
    }

    public boolean allocateRunway() {
        try {
            if (lock.tryLock()) {
                isOccupied = true;
                return true;
            }
            return false;
        } catch (Exception e) {
            return false;
        }
    }

    public boolean releaseRunway() {
        try {
            isOccupied = false;
            lock.unlock();
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    public String getId() {
        return id;
    }

    public boolean isFree() {
        return !isOccupied;
    }

    @Override
    public String toString() {
        return "Runway{" +
                "id='" + id + '\'' +
                ", isOccupied=" + isOccupied +
                '}';
    }
}
