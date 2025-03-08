package design_patterns.creational.builder;

public class User {
    private String id;
    private String name;
    private String email;
    private String phone;
    private String address;
    private String pin;
    private String city;
    private String state;
    private String country;

    User(UserBuilder userBuilder) {
        this.id = userBuilder.getId();
        this.name = userBuilder.getName();
        this.email = userBuilder.getEmail();
        this.phone = userBuilder.getPhone();
        this.address = userBuilder.getAddress();
        this.pin = userBuilder.getPin();
        this.city = userBuilder.getCity();
        this.state = userBuilder.getState();
        this.country = userBuilder.getCountry();
    }

    @Override
    public String toString() {
        return "User{" +
                "id='" + id + '\'' +
                ", name='" + name + '\'' +
                ", email='" + email + '\'' +
                ", phone='" + phone + '\'' +
                ", address='" + address + '\'' +
                ", pin='" + pin + '\'' +
                ", city='" + city + '\'' +
                ", state='" + state + '\'' +
                ", country='" + country + '\'' +
                '}';
    }
}
