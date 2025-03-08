package design_problems.text_pad.commands;

public interface Command {
    void execute();
    void undo();
}
