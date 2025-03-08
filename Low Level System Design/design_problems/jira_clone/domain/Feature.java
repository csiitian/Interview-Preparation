package design_problems.jira_clone.domain;

import design_problems.jira_clone.status.FeatureWorkflowStrategy;

import java.time.LocalDate;

public class Feature extends Task {
    private String featureSummary;
    private Impact impact;

    public Feature(String title, User creator, User assignee, TaskStatus status, TaskType type, LocalDate dueDate, Sprint sprint, String featureSummary, Impact impact) {
        super(title, creator, assignee, status, type, dueDate, sprint);
        this.featureSummary = featureSummary;
        this.impact = impact;
        setWorkflowStrategy();
    }

    public String getFeatureSummary() {
        return featureSummary;
    }

    public void setFeatureSummary(String featureSummary) {
        this.featureSummary = featureSummary;
    }

    public Impact getImpact() {
        return impact;
    }

    public void setImpact(Impact impact) {
        this.impact = impact;
    }

    @Override
    void setWorkflowStrategy() {
        this.workflowStrategy = new FeatureWorkflowStrategy();
    }

    @Override
    public String toString() {
        return "Feature{" +
                super.toString() + ", " +
                "featureSummary='" + featureSummary + '\'' +
                ", impact=" + impact +
                '}';
    }
}
