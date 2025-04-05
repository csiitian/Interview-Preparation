package design_problems.rate_limiter.algorithms;

abstract class Algorithm {
    Long maxRequestCount;
    Long interval;

    Algorithm(Long maxRequestCount, Long interval) {
        this.maxRequestCount = maxRequestCount;
        this.interval = interval;
    }

    abstract boolean allowRequest(String userId);
}
