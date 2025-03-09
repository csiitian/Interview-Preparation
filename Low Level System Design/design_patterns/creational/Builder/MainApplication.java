package design_patterns.creational.Builder;

public class MainApplication {

  public static void main(String[] args) {

    User user = new UserBuilder()
        .setUserName("Vikasss_7663")
        // .setEmail("vikas@gmail.com")
        .setPhone("+91 1234567890")
        .build();

    System.out.println(user);
  }
}
