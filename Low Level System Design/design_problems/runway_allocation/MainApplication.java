package design_problems.runway_allocation;

import design_problems.runway_allocation.strategy.FirstAvailableRunwayAllocationStrategy;

import java.time.Duration;
import java.util.List;

public class MainApplication {
    public static void main(String[] args) {
        Runway runway1 = new Runway("1");
        Runway runway2 = new Runway("2");
        Runway runway3 = new Runway("3");
        List<Runway> runwayList = List.of(
            runway1, runway2, runway3
        );
        ControlTower controlTower = new ControlTower(
                runwayList,
                new FirstAvailableRunwayAllocationStrategy(runwayList)
        );

        controlTower.requestRunway(new Plane("1"));
        controlTower.requestRunway(new Plane("2"));
        controlTower.requestRunway(new Plane("3"));
        controlTower.requestRunway(new Plane("4"));
        controlTower.requestRunway(new Plane("5"));

        controlTower.releaseRunway(runway1);
        controlTower.releaseRunway(runway3);

        try {
            Thread.sleep(Duration.ofSeconds(5));
        } catch (InterruptedException e) {
            System.out.println(e.getMessage());
        }
    }
}
