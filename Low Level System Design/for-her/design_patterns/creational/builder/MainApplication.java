package design_patterns.creational.builder;

import java.util.UUID;

public class MainApplication {
    public static void main(String[] args) {
        User user = new UserBuilder()
                .setId(UUID.randomUUID().toString())
                .setName("Vishal")
                .setCity("Jhunjhunu")
                .setState("Rajasthan")
                .setCountry("India")
                .build();

        System.out.println(user);
    }
}
