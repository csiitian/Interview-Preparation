package design_problems.task_management;

import java.time.LocalDateTime;

public class Task {
    String taskId;
    public String title;
    public String description;
    public TaskStatus taskStatus;
    public TaskPriority taskPriority;
    User createdBy;
    User assignedTo;
    LocalDateTime createdAt;
    LocalDateTime updatedAt;

    @Override
    public String toString() {
        return "Task{" +
                "taskId='" + taskId + '\'' +
                ", title='" + title + '\'' +
                ", description='" + description + '\'' +
                ", taskStatus='" + taskStatus.name() + '\'' +
                ", taskPriority='" + taskPriority.name() + '\'' +
                ", createdBy=" + createdBy +
                ", assignedTo=" + assignedTo +
                '}';
    }
}
