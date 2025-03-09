package design_patterns.structural.adapter;

import java.util.ArrayList;
import java.util.List;

public class Adapter implements IAdapter {
    IClient client;

    public Adapter(IClient client) {
        this.client = client;
    }

    @Override
    public List<User> fetchUsers() {
        String userStr = client.fetchUsers();
        String[] users = userStr.split(";");
        List<User> userList = new ArrayList<>();
        for (String user: users) {
            String[] userParams = user.split(",");
            if (userParams.length == 2) {
                User userDto = new User(userParams[0].trim(), userParams[1].trim());
                userList.add(userDto);
            } else {
                throw new RuntimeException("[" + userStr + "] is not formatted.");
            }
        }
        return userList;
    }
}
