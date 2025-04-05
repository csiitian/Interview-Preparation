package design_problems.flip_fit.domain;

import java.util.List;

public class Center {
    String id;
    String name;
    String location;
    Long capacity; // capacity per slot per workout
    List<Slot> slots;
}
