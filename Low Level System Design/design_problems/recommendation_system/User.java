package design_problems.recommendation_system;

import java.util.List;

public class User {
    private String id;
    private String name;
    private String email;
    private String password;
    private List<String> preferencesList;
    private List<Interaction> interactions;

    public String getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public String getEmail() {
        return email;
    }

    public String getPassword() {
        return password;
    }

    public List<String> getPreferencesList() {
        return preferencesList;
    }

    public List<Interaction> getInteractions() {
        return interactions;
    }
}
