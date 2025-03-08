package design_problems.recommendation_system;

import java.util.List;

public class Product {
    private String id;
    private String name;
    private String category;
    private List<String> tags;

    public String getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public String getCategory() {
        return category;
    }

    public List<String> getTags() {
        return tags;
    }
}
