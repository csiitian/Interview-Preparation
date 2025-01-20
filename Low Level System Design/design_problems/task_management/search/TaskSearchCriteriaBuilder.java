package design_problems.task_management.search;

import java.util.ArrayList;
import java.util.List;

public class TaskSearchCriteriaBuilder {
    private final List<TaskSpecification> specifications = new ArrayList<>();

    public TaskSearchCriteriaBuilder withKeyword(String keyword) {
        if (keyword != null) {
            specifications.add((task) -> task.title.contains(keyword) || task.description.contains(keyword));
        }
        return this;
    }

    public TaskSearchCriteriaBuilder withStatus(String taskStatus) {
        if (taskStatus != null) {
            specifications.add((task) -> task.taskStatus.name().equals(taskStatus));
        }
        return this;
    }

    public TaskSearchCriteriaBuilder withPriority(String taskPriority) {
        if (taskPriority != null) {
            specifications.add((task) -> task.taskPriority.name().equals(taskPriority));
        }
        return this;
    }

    public TaskSpecification build() {
        return task -> specifications.stream().allMatch(spec -> spec.isSatisfied(task));
    }
}
