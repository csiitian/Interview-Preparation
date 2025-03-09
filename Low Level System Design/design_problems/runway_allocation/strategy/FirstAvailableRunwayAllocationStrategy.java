package design_problems.runway_allocation.strategy;

import design_problems.runway_allocation.Plane;
import design_problems.runway_allocation.Runway;

import java.util.List;
import java.util.Optional;

public class FirstAvailableRunwayAllocationStrategy implements RunwayAllocationStrategy {
    List<Runway> runwayList;

    public FirstAvailableRunwayAllocationStrategy(List<Runway> runwayList) {
        this.runwayList = runwayList;
    }

    @Override
    public Optional<Runway> assignRunway(Plane plane) {
        return runwayList.stream()
                .filter(runway -> runway.isFree() && runway.allocateRunway())
                .findAny();
    }
}
