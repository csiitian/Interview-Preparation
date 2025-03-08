package design_patterns.structural.composite;

public abstract class Employee {
    String name;
    String designation;

    public Employee(String name, String designation) {
        this.name = name;
        this.designation = designation;
    }

    abstract String showDetails();

    String getFullName() {
        return "⚪ " + name + " ( " + designation + " )";
    }
}
