package design_problems.text_pad.commands;

import design_problems.text_pad.TextPad;

public class InsertCommand implements Command {
    TextPad textPad;
    int line;
    String text;

    public InsertCommand(TextPad textPad, int line, String text) {
        this.textPad = textPad;
        this.line = line;
        this.text = text;
    }

    @Override
    public void execute() {
        textPad.getContent().add(line, text);
    }

    @Override
    public void undo() {
        textPad.getContent().remove(line);
    }
}
