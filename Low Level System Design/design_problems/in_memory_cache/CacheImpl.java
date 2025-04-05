package design_problems.in_memory_cache;

import design_problems.in_memory_cache.eviction.CacheEvictionStrategy;

import java.util.Map;
import java.util.Optional;
import java.util.concurrent.ConcurrentHashMap;

public class CacheImpl<K, V> implements Cache<K, V> {
    final Map<K, V> cache;
    CacheEvictionStrategy<K> cacheEvictionStrategy;

    public CacheImpl(CacheEvictionStrategy<K> cacheEvictionStrategy) {
        this.cache = new ConcurrentHashMap<>();
        this.cacheEvictionStrategy = cacheEvictionStrategy;
    }

    @Override
    public Optional<V> get(K key) {
        return Optional.empty();
    }

    @Override
    public void put(K key, V value) {
    }

    @Override
    public void remove(K key) {

    }
}
