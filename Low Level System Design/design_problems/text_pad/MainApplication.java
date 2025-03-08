package design_problems.text_pad;

public class MainApplication {
    public static void main(String[] args) {
        TextPad textPad = new TextPad();
        textPad.addLine(0, "How are you ?");
        textPad.addLine(1, "I am doing good.");
        textPad.addLine(0, "Hey");
        textPad.display();

        textPad.undo();
        textPad.display();

        textPad.undo();
        textPad.display();

        textPad.addLine(0, "Test Display");

        textPad.redo();
        textPad.display();

        textPad.redo();
        textPad.display();

        textPad.redo();
        textPad.display();
    }
}
