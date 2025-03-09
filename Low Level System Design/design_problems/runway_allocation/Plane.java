package design_problems.runway_allocation;

public class Plane {
    private String id;

    public Plane(String id) {
        this.id = id;
    }

    @Override
    public String toString() {
        return "Plane{" +
                "id='" + id + '\'' +
                '}';
    }
}
