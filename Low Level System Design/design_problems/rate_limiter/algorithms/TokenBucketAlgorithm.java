package design_problems.rate_limiter.algorithms;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicLong;

public class TokenBucketAlgorithm extends Algorithm {
    final Long bucketSize;
    final Long refillRate; // tokens per second
    Map<String, AtomicLong> currentTokenMap;
    Map<String, Long> lastRefillTimestamp;

    TokenBucketAlgorithm(Long maxRequestCount, Long interval, Long bucketSize, Long refillRate) {
        super(maxRequestCount, interval);
        this.bucketSize = bucketSize;
        this.refillRate = refillRate;
        this.currentTokenMap = new ConcurrentHashMap<>();
        this.lastRefillTimestamp = new ConcurrentHashMap<>();
    }

    public void refillTokens(String userId) {
        Long now = System.currentTimeMillis();
        if (!lastRefillTimestamp.containsKey(userId)) {
            lastRefillTimestamp.put(userId, now);
            currentTokenMap.put(userId, new AtomicLong(bucketSize));
        }
        Long lastRefill = lastRefillTimestamp.get(userId);
        long elapsedTimeInSeconds = (now - lastRefill ) / 1000L;
        if (elapsedTimeInSeconds > 0L) {
            long newTokens = elapsedTimeInSeconds * refillRate;
            lastRefillTimestamp.put(userId, now);
            AtomicLong currentToken = currentTokenMap.get(userId);
            currentToken.addAndGet(Math.min(bucketSize, currentToken.get() + newTokens));
        }
    }

    @Override
    public boolean allowRequest(String userId) {
        refillTokens(userId);
        AtomicLong tokens = currentTokenMap.get(userId);
        if (tokens.get() > 0) {
            tokens.decrementAndGet();
            return true;
        }
        return false;
    }
}
