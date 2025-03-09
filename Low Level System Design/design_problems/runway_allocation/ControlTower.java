package design_problems.runway_allocation;

import design_problems.runway_allocation.strategy.RunwayAllocationStrategy;

import java.util.LinkedList;
import java.util.List;
import java.util.Optional;

public class ControlTower {
    List<Runway> runwayList;
    RunwayAllocationStrategy runwayAllocationStrategy;
    List<Plane> waitingList;

    public ControlTower(List<Runway> runwayList, RunwayAllocationStrategy runwayAllocationStrategy) {
        this.runwayList = runwayList;
        this.runwayAllocationStrategy = runwayAllocationStrategy;
        this.waitingList = new LinkedList<>();
    }

    public void setRunwayAllocationStrategy(RunwayAllocationStrategy runwayAllocationStrategy) {
        this.runwayAllocationStrategy = runwayAllocationStrategy;
    }

    public synchronized void requestRunway(Plane plane) {
        Optional<Runway> runwayOptional = runwayAllocationStrategy.assignRunway(plane);
        if (runwayOptional.isPresent()) {
            System.out.println("Runway " + runwayOptional.get().getId() + " is assigned to " + plane);
            System.out.println(runwayOptional.get());
        } else {
            waitingList.add(plane);
        }
    }

    public synchronized void releaseRunway(Runway runway) {
        if (runway.releaseRunway()) {
            System.out.println(runway.getId() + " is free.");

            if (!waitingList.isEmpty()) {
                Plane waitingPlane = waitingList.removeFirst();
                requestRunway(waitingPlane);
            }
        } else {
            System.out.println("not able to release it.");
        }
    }
}
