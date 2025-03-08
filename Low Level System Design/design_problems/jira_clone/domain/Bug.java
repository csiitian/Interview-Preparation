package design_problems.jira_clone.domain;

import design_problems.jira_clone.status.BugWorkflowStrategy;

import java.time.LocalDate;

public class Bug extends Task {
    private BugSeverity severity;

    public Bug(String title, User creator, User assignee, TaskStatus status, TaskType type, LocalDate dueDate, Sprint sprint, BugSeverity severity) {
        super(title, creator, assignee, status, type, dueDate, sprint);
        this.severity = severity;
        setWorkflowStrategy();
    }

    @Override
    void setWorkflowStrategy() {
        this.workflowStrategy = new BugWorkflowStrategy();
    }

    @Override
    public String toString() {
        return "Bug{" +
                super.toString() + ", " +
                "severity=" + severity +
                '}';
    }
}
