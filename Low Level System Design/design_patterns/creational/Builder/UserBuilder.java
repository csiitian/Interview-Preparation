package design_patterns.creational.Builder;

public class UserBuilder {
  String userName;
  String email;
  String phone;

  UserBuilder() {
  }

  public UserBuilder setUserName(String userName) {
    this.userName = userName;
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

  public User build() {
    return new User(this);
  }
}
