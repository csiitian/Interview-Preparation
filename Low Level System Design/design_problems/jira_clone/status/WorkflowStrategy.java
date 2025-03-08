package design_problems.jira_clone.status;

import design_problems.jira_clone.domain.TaskStatus;

import java.util.Set;

public interface WorkflowStrategy {
    boolean isValidTransition(TaskStatus currentStatus, TaskStatus newStatus);
}
