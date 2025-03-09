package design_patterns.behavioral.iterator;

import java.util.List;

public class MainApplication {
    public static void main(String[] args) {
        List<Student> studentList = List.of(
                new Student("1001", "Kapil Singh"),
                new Student("1002", "Satish Singh"),
                new Student("1003", "Sachin Singh"),
                new Student("1004", "Vishal Singh"),
                new Student("1005", "Vikas Singh")
        );
        Batch batch = new Batch("ECE", studentList);
        BatchIterator batchIterator = batch.iterator();
        while (batchIterator.hasNext()) {
            System.out.println(batchIterator.next());
        }
    }
}
