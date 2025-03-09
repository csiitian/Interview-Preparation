package design_problems.logger_framework;

import java.io.FileWriter;
import java.io.IOException;

public class FileLogAppender implements LogAppender {
    private final String filePath;

    public FileLogAppender(String filePath) {
        this.filePath = filePath;
    }

    @Override
    public void append(String message) {
        try (FileWriter fileWriter = new FileWriter(filePath, true)) {
            fileWriter.write(message + "\n");
        } catch (IOException e) {
            System.out.println(e.getMessage());
        }
    }
}
