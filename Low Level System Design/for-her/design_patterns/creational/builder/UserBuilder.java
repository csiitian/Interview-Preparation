package design_patterns.creational.builder;

public class UserBuilder {
    private String id;
    private String name;
    private String email;
    private String phone;
    private String address;
    private String pin;
    private String city;
    private String state;
    private String country;
    
    public UserBuilder setId(String id) {
        this.id = id;
        return this;
    }
    
    public UserBuilder setName(String name) {
        this.name = name;
        return this;
    }
    
    public UserBuilder setEmail(String email) {
        this.email = email;
        return this;
    }

    public UserBuilder setPhone(String phone) {
        this.phone = phone;
        return this;
    }

    public UserBuilder setAddress(String address) {
        this.address = address;
        return this;
    }

    public UserBuilder setPin(String pin) {
        this.pin = pin;
        return this;
    }

    public UserBuilder setCity(String city) {
        this.city = city;
        return this;
    }

    public UserBuilder setState(String state) {
        this.state = state;
        return this;
    }

    public UserBuilder setCountry(String country) {
        this.country = country;
        return this;
    }

    public User build() {
        return new User(this);
    }

    public String getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public String getEmail() {
        return email;
    }

    public String getPhone() {
        return phone;
    }

    public String getAddress() {
        return address;
    }

    public String getPin() {
        return pin;
    }

    public String getCity() {
        return city;
    }

    public String getState() {
        return state;
    }

    public String getCountry() {
        return country;
    }
}
