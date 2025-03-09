package design_patterns.structural.adapter;

public class Client implements IClient {
    @Override
    public String fetchUsers() {
        return "u123, John Doe; u456, Jane Doe; u789, Bob Smith";
    }
}
