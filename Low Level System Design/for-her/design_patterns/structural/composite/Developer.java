package design_patterns.structural.composite;

public class Developer extends Employee {

    public Developer(String name, String designation) {
        super(name, designation);
    }

    @Override
    String showDetails() {
        return getFullName();
    }
}
