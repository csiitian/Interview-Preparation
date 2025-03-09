package design_problems.task_management;

import design_problems.task_management.search.SearchCriteria;
import design_problems.task_management.search.TaskSearchCriteriaBuilder;
import design_problems.task_management.search.TaskSpecification;

import java.util.*;

public class TaskManager {
    static volatile TaskManager instance;
    Map<String, User> users;
    Map<String, Task> tasks;
    final Object lock;

    private TaskManager() {
        users = new HashMap<>();
        tasks = new HashMap<>();
        lock = new Object();
    }

    public synchronized static TaskManager getInstance() {
        if (instance == null) {
            instance = new TaskManager();
        }
        return instance;
    }

    public User createUser(String name, String email) {
        String userId = "User:" + UUID.randomUUID();
        User user = new User();
        user.userId = userId;
        user.name = name;
        user.email = email;
        users.put(userId, user);
        return user;
    }

    public Task createTask(String title, String description, TaskPriority taskPriority, User createdBy, User assignedTo) {
        Task task = new Task();
        String taskId = "Task:" + UUID.randomUUID();
        task.taskId = taskId;
        task.title = title;
        task.description = description;
        task.taskStatus = TaskStatus.BACKLOG;
        task.taskPriority = taskPriority;
        task.createdBy = createdBy;
        task.assignedTo = assignedTo == null ? createdBy: assignedTo;
        tasks.put(taskId, task);
        return task;
    }

    public void completeTask(Task task) {
        if (task == null) {
            throw new RuntimeException("Task not found.");
        }
        synchronized (lock) {
           task.taskStatus = TaskStatus.COMPLETED;
        }
    }

    public List<Task> getTasks(SearchCriteria searchCriteria) {
        TaskSpecification taskSpecification = new TaskSearchCriteriaBuilder()
                .withKeyword(searchCriteria.keyword)
                .withStatus(searchCriteria.taskStatus)
                .withPriority(searchCriteria.taskPriority)
                .build();

        return tasks.values()
                .stream()
                .filter(taskSpecification::isSatisfied)
                .toList();
    }
}
