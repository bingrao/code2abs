@org.junit.Test(expected = javax.cache.CacheException.class)
public void whenInvalidConfigLocationAsUri_thenFails() throws java.net.URISyntaxException {
    com.hazelcast.cache.HazelcastCacheManager cacheManager = ((com.hazelcast.cache.HazelcastCacheManager) (cachingProvider.getCacheManager(new java.net.URI("classpath:this-config-does-not-exist"), null)));
}