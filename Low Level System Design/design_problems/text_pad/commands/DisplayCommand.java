package design_problems.text_pad.commands;

import design_problems.text_pad.TextPad;

import java.util.List;

public class DisplayCommand implements Command {
    TextPad textPad;

    public DisplayCommand(TextPad textPad) {
        this.textPad = textPad;
    }


    @Override
    public void execute() {
        List<String> content = textPad.getContent();
        System.out.println("Display:");
        for (String line: content) {
            System.out.println(line);
        }
    }

    @Override
    public void undo() {
        // nothing to process here
    }
}
