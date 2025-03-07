void main() {
  Hello.sayHello();
  Pen pen1 = new Pen(5, "Butter flow");
  Pen pen2 = new Pen(10, "Butter Flow");
  if(pen1.equals(pen2)) {
    System.out.println("Both have same brand");
  }
}

static class Hello {
  static void sayHello() {
    System.out.println("hello world !!!");
  }
}

class Pen {

  int price;
  String brand;

  Pen(int price, String brand) {
    this.price = price;
    this.brand = brand;
  }

  @Override
  public boolean equals(Object obj) {
    Pen other = (Pen) obj;
    return this.brand.equalsIgnoreCase(other.brand);
  }
}

