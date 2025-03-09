package design_patterns.behavioral.iterator;


import java.util.List;

public class Batch {
    String name;
    List<Student> students;

    public Batch(String name, List<Student> students) {
        this.name = name;
        this.students = students;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public List<Student> getStudents() {
        return students;
    }

    public void setStudents(List<Student> students) {
        this.students = students;
    }

    public BatchIterator iterator() {
        return new BatchIterator(this);
    }
}
