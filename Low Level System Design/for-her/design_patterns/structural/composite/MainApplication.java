package design_patterns.structural.composite;

import java.util.List;

public class MainApplication {
    public static void main(String[] args) {
        Employee developer1 = new Developer("alice", "Software Engineer");
        Employee developer2 = new Developer("bob", "Senior Software Engineer");
        Employee l1Manager = new Manager("charlie", "Technical Lead", List.of(developer1, developer2));
        Employee l2Manager = new Manager("dodge", "Technical Program Manager", List.of(l1Manager));
        System.out.println(l2Manager.showDetails());
    }
}
