package design_patterns.structural.adapter;

import java.util.List;

public class MainApplication {
    public static void main(String[] args) {
        IClient client = new Client();
        IAdapter adapter = new Adapter(client);
        try {
            List<User> userList = adapter.fetchUsers();
            System.out.println(userList);
        } catch (Exception e) {
            System.err.println(e.getMessage());
        }
    }
}
