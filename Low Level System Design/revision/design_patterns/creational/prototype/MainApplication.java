package design_patterns.creational.prototype;

public class MainApplication {
    public static void main(String[] args) {
        Student student = new Student(1001, "Vishal Singh");
        Student clonedStudent = (Student) student.clone();
        System.out.println(clonedStudent);
    }
}
