package design_problems.jira_clone;

import design_problems.jira_clone.domain.*;

import java.time.LocalDate;

public class Main {

  public static void main(String[] args) {
    TaskPlanner taskPlanner = new TaskPlanner();
    User user1 = taskPlanner.addUser("vishal");
    User user2 = taskPlanner.addUser("satish");
    Bug bug = new Bug(
            "Page not loading",
            user1,
            user2,
            TaskStatus.OPEN,
            TaskType.BUG,
            LocalDate.now().plusDays(5),
            null,
            BugSeverity.MODERATE
    );
    Feature feature = new Feature(
            "add pagination on ui",
            user2,
            user1,
            TaskStatus.OPEN,
            TaskType.FEATURE,
            LocalDate.now().plusDays(10),
            null,
            "This feature will fix the UI loading issue",
            Impact.HIGH
    );
    taskPlanner.addTask(bug);
    taskPlanner.addTask(feature);

    taskPlanner.changeStatus(bug, TaskStatus.IN_PROGRESS);
    taskPlanner.changeStatus(bug, TaskStatus.IN_PROGRESS);

    taskPlanner.printAllTasks();
  }
}