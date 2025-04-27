package design_problems.car_rental_system.domain;

public class User {
  Long userId;
  String name;
  String email;
  String mobile;
  String address;
  String drivingLicense;
  String password;
  UserType type;

  public User(Long userId, String name, String email, String mobile, String address, String drivingLicense, String password, UserType type) {
    this.userId = userId;
    this.name = name;
    this.email = email;
    this.mobile = mobile;
    this.address = address;
    this.drivingLicense = drivingLicense;
    this.password = password;
    this.type = type;
  }

  public UserType getType() {
    return type;
  }

  public String getPassword() {
    return password;
  }

  public String getDrivingLicense() {
    return drivingLicense;
  }

  public String getMobile() {
    return mobile;
  }

  public Long getUserId() {
    return userId;
  }

  public String getName() {
    return name;
  }

  public String getEmail() {
    return email;
  }

  public String getAddress() {
    return address;
  }
}
