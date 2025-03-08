package design_problems.jira_clone;

import design_problems.jira_clone.domain.*;
import design_problems.jira_clone.exception.InvalidTaskStatusException;
import design_problems.jira_clone.exception.TaskNotFoundException;
import design_problems.jira_clone.exception.UserNotFoundException;

import java.util.ArrayList;
import java.util.List;

public class TaskPlanner {
    private final List<Task> tasks;
    private final List<User> users;

    TaskPlanner() {
        this.tasks = new ArrayList<>();
        this.users = new ArrayList<>();
    }

    private User getUserByName(String name) {
        for (User user: users) {
            if (user.getName().equals(name)) {
                return user;
            }
        }
        return null;
    }

    private Task getTaskByTitle(String name) {
        for (Task task: tasks) {
            if (task.getTitle().equals(name)) {
                return task;
            }
        }
        return null;
    }

    public User addUser(String name) {
        User existingUser = getUserByName(name);
        if (existingUser == null ) {
            User user = new User(name);
            users.add(user);
            return user;
        }
        return existingUser;
    }

    public void addTask(Task task) {
        tasks.add(task);
    }

    private void assignedTo(String taskTitle, String assignee) {
        User user = getUserByName(assignee);
        if (user == null) {
            throw new UserNotFoundException();
        }
        Task task = getTaskByTitle(taskTitle);
        if (task == null) {
            throw new TaskNotFoundException();
        }
        task.setAssignee(user);
    }

    public void printAllTasks() {
        tasks.forEach(System.out::println);
    }

    public void changeStatus(Task task, TaskStatus taskStatus) {
        try {
            task.changeStatus(taskStatus);
        } catch (InvalidTaskStatusException e) {
            System.out.println(e.getMessage());
        }
    }
}
