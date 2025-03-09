package design_patterns.creational.Prototype;

public class MainApplication {

  public static void main(String[] args) {
    Student student = new Student("John", 20, 123);

    Student clonedStudent = student.clone();
    System.out.println(clonedStudent.getName());
    System.out.println(clonedStudent.getAge());
    System.out.println(clonedStudent.getRollNo());
  }
}
