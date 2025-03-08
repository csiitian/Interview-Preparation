package design_problems.text_pad;

import design_problems.text_pad.commands.*;

import java.util.ArrayList;
import java.util.List;
import java.util.Stack;

public class TextPad {
    private final List<String> content;
    private final Stack<Command> undoStack;
    private final Stack<Command> redoStack;

    TextPad() {
        this.content = new ArrayList<>();
        this.undoStack = new Stack<>();
        this.redoStack = new Stack<>();
    }

    public List<String> getContent() {
        return content;
    }

    public void addLine(int line, String text) {
        InsertCommand command = new InsertCommand(this, line, text);
        command.execute();
        undoStack.push(command);
        redoStack.clear();
    }

    public void deleteLine(int line) {
        DeleteCommand command = new DeleteCommand(this, line);
        command.execute();
        undoStack.push(command);
        redoStack.clear();
    }

    public void display() {
        DisplayCommand command = new DisplayCommand(this);
        command.execute();
    }

    public void undo() {
        if (undoStack.isEmpty()) {
            System.out.println("no undo command to process");
            return;
        }
        Command command = undoStack.pop();
        redoStack.push(command);
        command.undo();
    }

    public void redo() {
        if (redoStack.isEmpty()) {
            System.out.println("no redo command to process");
            return;
        }
        Command command = redoStack.pop();
        undoStack.push(command);
        command.execute();
    }
}
