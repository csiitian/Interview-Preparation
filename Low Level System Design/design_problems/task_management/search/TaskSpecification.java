package design_problems.task_management.search;

import design_problems.task_management.Task;

@FunctionalInterface
public interface TaskSpecification {
    boolean isSatisfied(Task task);
}
