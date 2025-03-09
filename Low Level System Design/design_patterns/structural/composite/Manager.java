package design_patterns.structural.composite;

import java.util.List;

public class Manager extends Employee {
    List<Employee> employeeList;

    public Manager(String name, String designation, List<Employee> employeeList) {
        super(name, designation);
        this.employeeList = employeeList;
    }

    @Override
    String showDetails() {
        StringBuilder sb = new StringBuilder();
        sb.append(getFullName());
        sb.append("\n");
        for (Employee employee : employeeList) {
            employee.showDetails().lines()
                    .forEach(details -> sb.append("\t").append(details).append("\n"));
        }
        return sb.toString();
    }
}
