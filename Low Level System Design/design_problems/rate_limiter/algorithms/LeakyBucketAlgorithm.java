package design_problems.rate_limiter.algorithms;

public class LeakyBucketAlgorithm extends Algorithm {
    Long leakRate;

    LeakyBucketAlgorithm(Long maxRequestCount, Long interval) {
        super(maxRequestCount, interval);
    }

    @Override
    public boolean allowRequest(String userId) {
        return false;
    }
}
