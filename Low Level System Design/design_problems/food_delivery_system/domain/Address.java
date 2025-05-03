package design_problems.food_delivery_system.domain;

public class Address {
    private String line1;
    private String line2;
    private String city;
    private String state;
    private String zip;

    public Address(String line1, String line2, String city, String state, String zip) {
        this.line1 = line1;
        this.line2 = line2;
        this.city = city;
        this.state = state;
        this.zip = zip;
    }
}
