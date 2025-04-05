package design_problems.rate_limiter.algorithms;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

public class FixedWindowCounterAlgorithm extends Algorithm {
    Long startWindowTime;
    Map<String, AtomicLong> requestCountMap;

    FixedWindowCounterAlgorithm(Long maxRequestCount, Long interval) {
        super(maxRequestCount, interval);
        startWindowTime = System.currentTimeMillis();
        requestCountMap = new ConcurrentHashMap<>();
    }

    @Override
    public boolean allowRequest(String userId) {
        Long currentTime = System.currentTimeMillis();
        if (currentTime - startWindowTime > interval) {
            startWindowTime = currentTime;
            requestCountMap.clear();
        }
        return requestCountMap
                .computeIfAbsent(userId, _ -> new AtomicLong())
                .incrementAndGet() <= maxRequestCount;
    }
}
