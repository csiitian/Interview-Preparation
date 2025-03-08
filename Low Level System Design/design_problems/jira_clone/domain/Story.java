package design_problems.jira_clone.domain;

import design_problems.jira_clone.status.StoryWorkflowStrategy;

import java.time.LocalDate;
import java.util.List;

public class Story extends Task {
    private String storySummary;
    private List<Subtask> subtasks;
    private SubTrack subtrack;

    public Story(String title, User creator, User assignee, TaskStatus status, TaskType type, LocalDate dueDate, Sprint sprint, String storySummary, List<Subtask> subtasks, SubTrack subtrack) {
        super(title, creator, assignee, status, type, dueDate, sprint);
        this.storySummary = storySummary;
        this.subtasks = subtasks;
        this.subtrack = subtrack;
        setWorkflowStrategy();
    }

    @Override
    void setWorkflowStrategy() {
        this.workflowStrategy = new StoryWorkflowStrategy();
    }

    @Override
    public String toString() {
        return "Story{" +
                super.toString() + "," +
                "storySummary='" + storySummary + '\'' +
                ", subtasks=" + subtasks +
                ", subtrack=" + subtrack +
                '}';
    }
}
