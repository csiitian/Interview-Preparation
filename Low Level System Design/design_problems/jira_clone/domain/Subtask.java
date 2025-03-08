package design_problems.jira_clone.domain;

import design_problems.jira_clone.status.SubtaskWorkflowStrategy;

import java.time.LocalDate;

public class Subtask extends Task {
    public Subtask(String title, User creator, User assignee, TaskStatus status, TaskType type, LocalDate dueDate, Sprint sprint) {
        super(title, creator, assignee, status, type, dueDate, sprint);
        setWorkflowStrategy();
    }

    @Override
    void setWorkflowStrategy() {
        this.workflowStrategy = new SubtaskWorkflowStrategy();
    }

    @Override
    public String toString() {
        return "Subtask{" +
                super.toString() +
                "}";
    }
}
