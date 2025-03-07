package design_patterns.structural.proxy;

import java.util.List;

public class User {
    private String id;
    private String name;
    private List<AccessLevel> accessLevelList;

    public User(String id, String name, List<AccessLevel> accessLevelList) {
        this.id = id;
        this.name = name;
        this.accessLevelList = accessLevelList;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<AccessLevel> getAccessLevelList() {
        return accessLevelList;
    }

    public void setAccessLevelList(List<AccessLevel> accessLevelList) {
        this.accessLevelList = accessLevelList;
    }
}
