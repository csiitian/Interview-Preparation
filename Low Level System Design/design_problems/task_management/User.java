package design_problems.task_management;

import java.util.List;

public class User {
    String userId;
    String name;
    String email;
    List<Task> tasks;

    @Override
    public String toString() {
        return "User{" +
                "userId='" + userId + '\'' +
                ", name='" + name + '\'' +
                '}';
    }
}
