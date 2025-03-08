package design_problems.text_pad.commands;

import design_problems.text_pad.TextPad;

public class DeleteCommand implements Command {
    TextPad textPad;
    int line;
    String text;

    public DeleteCommand(TextPad textPad, int line) {
        this.textPad = textPad;
        this.line = line;
    }

    @Override
    public void execute() {
        this.text = textPad.getContent().remove(line);
    }

    @Override
    public void undo() {
        textPad.getContent().add(line, text);
    }
}
