package design_problems.rate_limiter.algorithms;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

public class SlidingWindowCounterAlgorithm extends Algorithm {

    SlidingWindowCounterAlgorithm(Long maxRequestCount, Long interval) {
        super(maxRequestCount, interval);
    }

    @Override
    public boolean allowRequest(String userId) {
        return true;
    }
}
