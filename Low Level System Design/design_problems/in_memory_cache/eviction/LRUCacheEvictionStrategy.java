package design_problems.in_memory_cache.eviction;

public class LRUCacheEvictionStrategy<K> implements CacheEvictionStrategy<K> {
    @Override
    public K evictKey(K key) {
        return null;
    }

    @Override
    public void recordAccess(K key) {

    }
}
