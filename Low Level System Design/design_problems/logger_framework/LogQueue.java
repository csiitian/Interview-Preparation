package design_problems.logger_framework;

import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;

public class LogQueue {
    BlockingQueue<Log> queue;

    public LogQueue() {
        this.queue = new LinkedBlockingQueue<>();
    }

    public void addLog(Log log) throws InterruptedException {
        queue.put(log);
    }

    public Log pollLog() throws InterruptedException {
        return queue.take();
    }
}
