package design_patterns.behavioral.iterator;

import java.util.Iterator;
import java.util.List;

public class BatchIterator implements Iterator<Student> {
    List<Student> students;
    int itr = 0;

    public BatchIterator(Batch batch) {
        this.students = batch.getStudents();
    }

    @Override
    public boolean hasNext() {
        return itr < students.size();
    }

    @Override
    public Student next() {
        if (hasNext()) {
            return students.get(itr++);
        } else {
            throw new RuntimeException("End of Iteration.");
        }
    }
}
