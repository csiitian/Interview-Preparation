package design_problems.task_management;

import design_problems.task_management.search.SearchCriteria;

public class MainApplication {
    public static void main(String[] args) {
        TaskManager taskManager = TaskManager.getInstance();
        User user1 = taskManager.createUser("Vishal Singh", "vishalsinghgk2018@gmail.com");
        User user2 = taskManager.createUser("Vikas Singh", "vishalsinghgk2021@gmail.com");
        Task task1 = taskManager.createTask("Drink Water", "Drink Water Daily", TaskPriority.HIGH, user1, null);
        Task task2 = taskManager.createTask("Leetcode Contest", "Weekly Contest", TaskPriority.MEDIUM, user2, null);
        Task task3 = taskManager.createTask("Leetcode Bi Weekly Contest", "Weekly Contest", TaskPriority.LOW, user2,
                user1);
        Task task4 = taskManager.createTask("Work", "Office Work", TaskPriority.LOW, user2, null);

        taskManager.completeTask(task1);
        taskManager.completeTask(task3);
        printTasks();
    }

    public static void printTasks() {
        TaskManager taskManager = TaskManager.getInstance();
        SearchCriteria searchCriteria = new SearchCriteria();
        searchCriteria.keyword = "Leetcode";
        searchCriteria.taskStatus = "COMPLETED";
        searchCriteria.taskPriority = "LOW";
        for (Task task: taskManager.getTasks(searchCriteria)) {
            System.out.println(task);
        }
    }
}
