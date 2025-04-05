package design_problems.in_memory_cache.eviction;

public interface CacheEvictionStrategy<K> {
    K evictKey(K key);
    void recordAccess(K key);
}
