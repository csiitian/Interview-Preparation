package design_problems.jira_clone.domain;

import design_problems.jira_clone.exception.InvalidTaskStatusException;
import design_problems.jira_clone.status.WorkflowStrategy;

import java.time.LocalDate;

public abstract class Task {
    private String title;
    private User creator;
    private User assignee;
    private TaskStatus status;
    private TaskType type;
    private LocalDate dueDate;
    private Sprint sprint;
    protected WorkflowStrategy workflowStrategy;

    public Task(String title, User creator, User assignee, TaskStatus status, TaskType type, LocalDate dueDate, Sprint sprint) {
        this.title = title;
        this.creator = creator;
        this.assignee = assignee;
        this.status = status;
        this.type = type;
        this.dueDate = dueDate;
        this.sprint = sprint;
    }

    abstract void setWorkflowStrategy();

    public String getTitle() {
        return title;
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public User getCreator() {
        return creator;
    }

    public void setCreator(User creator) {
        this.creator = creator;
    }

    public User getAssignee() {
        return assignee;
    }

    public void setAssignee(User assignee) {
        this.assignee = assignee;
    }

    public TaskStatus getStatus() {
        return status;
    }

    public void setStatus(TaskStatus status) {
        this.status = status;
    }

    public TaskType getType() {
        return type;
    }

    public void setType(TaskType type) {
        this.type = type;
    }

    public LocalDate getDueDate() {
        return dueDate;
    }

    public void setDueDate(LocalDate dueDate) {
        this.dueDate = dueDate;
    }

    @Override
    public String toString() {
        return "title='" + title + '\'' +
                ", creator=" + creator +
                ", assignee=" + assignee +
                ", status=" + status +
                ", type=" + type +
                ", dueDate=" + dueDate +
                ", sprint=" + sprint;
    }

    public void changeStatus(TaskStatus newStatus) {
        if (workflowStrategy.isValidTransition(this.status, newStatus)) {
            System.out.println(type + " moved from " + this.status + " to " + newStatus);
            setStatus(newStatus);
        } else {
            throw new InvalidTaskStatusException(type + " can't be moved from " + this.status + " to " + newStatus);
        }
    }
}
