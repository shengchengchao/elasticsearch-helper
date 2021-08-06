package com.xixi.search.common.lock;

import org.springframework.stereotype.Component;

import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.ReentrantLock;

/**
 * 提供加锁的方式
 * @author shengchengchao
 * @Description
 * @createTime 2021/7/29
 */
public class ElasticsearchLock {


    private static final Map<String, ReentrantLock> LOCAL_LOCK = new ConcurrentHashMap<>();


    /**
     * 得到锁 默认这个方法是不调用 要加锁 使用lock方法
     * @param key
     * @return
     */
    private static ReentrantLock getLock(String key){
        ReentrantLock reentrantLock = LOCAL_LOCK.get(key);
        if(reentrantLock== null){
            synchronized(ElasticsearchLock.class){
                reentrantLock = LOCAL_LOCK.get(key);
                if(reentrantLock ==null){
                    reentrantLock = new ReentrantLock();
                    LOCAL_LOCK.put(key,reentrantLock);
                }
            }
        }
        return reentrantLock;
    }

    public static Boolean tryLock(String key, long timeout, TimeUnit unit) throws Exception {
        ReentrantLock lock = null;
        try {
            lock = getLock(key);
            return lock.tryLock(timeout,unit);
        } catch (Exception e) {
            return false;
        }
    }

    public static Boolean tryLock(String key) {
        ReentrantLock lock = null;
        try {
            lock = getLock(key);
            return lock.tryLock(1,TimeUnit.SECONDS);
        } catch (Exception e) {
           return false;
        }

    }

    public static void lock(String key) throws Exception {
        ReentrantLock lock = null;
        try {
            lock = getLock(key);
            lock.lock();
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            lock.unlock();
        }
    }

    public static void unLock(String key){
        ReentrantLock lock = getLock(key);
        lock.unlock();
    }
}
