package design_problems.jira_clone.status;

import design_problems.jira_clone.domain.TaskStatus;

import java.util.*;

public class SubtaskWorkflowStrategy implements WorkflowStrategy {
    Map<TaskStatus, Set<TaskStatus>> workflowMap;

    public SubtaskWorkflowStrategy() {
        workflowMap = Map.of(
                TaskStatus.OPEN, Set.of(TaskStatus.IN_PROGRESS),
                TaskStatus.IN_PROGRESS, Set.of(TaskStatus.COMPLETED),
                TaskStatus.COMPLETED, Set.of(TaskStatus.DEPLOYED)
        );
    }

    @Override
    public boolean isValidTransition(TaskStatus currentStatus, TaskStatus newStatus) {
        return workflowMap.getOrDefault(currentStatus, Set.of()).contains(newStatus);
    }
}
