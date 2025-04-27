package concurrency.day_06;

public class MainApplication {
    public static void main(String[] args) {
        DeadlockSimulation deadlockSimulation = new DeadlockSimulation();
        new Thread(deadlockSimulation::processA).start();
        new Thread(deadlockSimulation::processB).start();
    }
}
