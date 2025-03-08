package design_problems.jira_clone.status;

import design_problems.jira_clone.domain.TaskStatus;
import design_problems.jira_clone.exception.InvalidTaskStatusException;

import java.util.*;

public class BugWorkflowStrategy implements WorkflowStrategy {
    Map<TaskStatus, Set<TaskStatus>> workflowMap;

    public BugWorkflowStrategy() {
        workflowMap = Map.of(
                TaskStatus.OPEN, Set.of(TaskStatus.IN_PROGRESS),
                TaskStatus.IN_PROGRESS, Set.of(TaskStatus.FIXED)
        );
    }

    @Override
    public boolean isValidTransition(TaskStatus currentStatus, TaskStatus newStatus) {
        return workflowMap.getOrDefault(currentStatus, Set.of()).contains(newStatus);
    }
}
