import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.stream.Stream;

void main() throws IOException {
  String filePatH = "/Users/vishalsingh/IdeaProjects/Interview Preparation/Low Level System Design/java_23/resources/DemoFile.txt";
  try (Stream<String> lines = Files.lines(Paths.get(filePatH)).onClose(new FileCloseRunner())) {
    lines.forEach(System.out::println);
  }
}

static class FileCloseRunner implements Runnable {
  @Override
  public void run() {
    System.out.println("File is closed.");
  }
}
