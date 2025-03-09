package design_patterns.creational.Builder;

public class User {
  private String userName;
  private String email;
  private String phone;

  public User() {
  }

  public User(UserBuilder userBuilder) {
    this.userName = userBuilder.userName;
    this.email = userBuilder.email;
    this.phone = userBuilder.phone;
  }

  public String getUserName() {
    return userName;
  }

  public void setUserName(String userName) {
    this.userName = userName;
  }

  public String getEmail() {
    return email;
  }

  public void setEmail(String email) {
    this.email = email;
  }

  public String getPhone() {
    return phone;
  }

  public void setPhone(String phone) {
    this.phone = phone;
  }

  @Override
  public String toString() {
    return "User{" +
        "userName='" + userName + '\'' +
        ", email='" + email + '\'' +
        ", phone='" + phone + '\'' +
        '}';
  }
}
