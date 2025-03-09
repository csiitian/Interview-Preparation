package design_problems.runway_allocation.strategy;

import design_problems.runway_allocation.Plane;
import design_problems.runway_allocation.Runway;

import java.util.Optional;

public interface RunwayAllocationStrategy {
    Optional<Runway> assignRunway(Plane plane);
}
