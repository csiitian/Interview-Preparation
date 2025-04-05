package design_problems.library_management.domain;


public class User {
  private String userId;
  private String name;
  private String email;
  private String phone;
  private UserType type;
  private UserStatus status;

  public User(String userId, String name, String email, String phone, UserType type, UserStatus status) {
    this.userId = userId;
    this.name = name;
    this.email = email;
    this.phone = phone;
    this.type = type;
    this.status = status;
  }

  public void blockUser() {
    this.status = UserStatus.BLOCKED;
  }

  public String getUserId() {
    return userId;
  }

  @Override
  public String toString() {
    return "{" + name + ", email: " + email + ", phone: " + phone + ", type: " + type + "}";
  }
}
