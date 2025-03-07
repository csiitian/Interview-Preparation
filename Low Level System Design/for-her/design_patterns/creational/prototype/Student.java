package design_patterns.creational.prototype;

public class Student implements IPrototype, Cloneable {
    private Integer rollNo;
    private String name;

    public Student(Integer rollNo, String name) {
        this.rollNo = rollNo;
        this.name = name;
    }

    @Override
    public IPrototype clone() {
        try {
            return (Student) super.clone();
        } catch (CloneNotSupportedException e) {
            throw new AssertionError();
        }
    }

    @Override
    public String toString() {
        return "Student{" +
                "name='" + name + '\'' +
                ", rollNo=" + rollNo +
                '}';
    }
}
