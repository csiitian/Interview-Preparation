package stream;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class StreamCollectExample {

  public static void main(String[] args) {

    List<Student> list = new ArrayList<>();
    list.add(new Student("Davis", SUBJECT.MATH, 1, 35.0));
    list.add(new Student("Davis", SUBJECT.SCIENCE, 2, 12.9));
    list.add(new Student("Davis", SUBJECT.MATH, 3, 37.0));
    list.add(new Student("Davis", SUBJECT.SCIENCE, 4, 37.0));
    list.add(new Student("Sascha", SUBJECT.ENGLISH, 5, 85.0));
    list.add(new Student("Sascha", SUBJECT.MATH, 1, 80.0));
    list.add(new Student("Sascha", SUBJECT.ENGLISH, 6, 12.0));
    list.add(new Student("Sascha", SUBJECT.MATH, 3, 50.0));
    list.add(new Student("Robert", SUBJECT.ENGLISH, 5, 12.0));
    Map<String, Map<SUBJECT, List<Double>>> map = new HashMap<>();
    list.stream().forEach(student -> {
      map.computeIfAbsent(student.getName(), s -> new HashMap<>())
          .computeIfAbsent(student.getSubject(), s -> new ArrayList<>())
          .add(student.getMarks());
    });
    System.out.println(map);
  }
}

enum SUBJECT {
  MATH,
  SCIENCE,
  ENGLISH
}

class Student {
  String name;
  SUBJECT subject;
  int semester;
  double marks;

  public Student(String name, SUBJECT subject, int semester, double marks) {
    this.name = name;
    this.subject = subject;
    this.marks = marks;
    this.semester = semester;
  }

  public String getName() {
    return name;
  }

  public void setName(String name) {
    this.name = name;
  }

  public SUBJECT getSubject() {
    return subject;
  }

  public void setSubject(SUBJECT subject) {
    this.subject = subject;
  }

  public int getSemester() {
    return semester;
  }

  public void setSemester(int semester) {
    this.semester = semester;
  }

  public double getMarks() {
    return marks;
  }

  public void setMarks(double marks) {
    this.marks = marks;
  }
}