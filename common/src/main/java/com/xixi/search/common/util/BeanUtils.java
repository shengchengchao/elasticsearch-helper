package com.xixi.search.common.util;

import com.google.common.collect.Lists;
import com.google.common.collect.Maps;
import org.springframework.cglib.beans.BeanCopier;
import org.springframework.cglib.core.Converter;

import java.util.List;
import java.util.Map;


public final class BeanUtils {

    public static final Map<String, BeanCopier> COPIER_MAP = Maps.newConcurrentMap();


    public static List copyList(Object[] source, Class targetClass){
        if (source == null) {
            return null;
        }
        List target = Lists.newArrayList();
        for(Object obj: source){
            target.add(copyProperties(obj,targetClass));
        }
        return target;
    }


    public static List copyList(List<?> source, Class targetClass){
        if (source == null) {
            return null;
        }
        List target = Lists.newArrayList();
        for(Object obj: source){
            target.add(copyProperties(obj,targetClass));
        }
        return target;
    }

    public static void copyProperties(Object source, Object target) {
        if (source == null) {
            return ;
        }
        BeanCopier copier = getBeanCopier(source.getClass(), target.getClass());
        copier.copy(source, target, null);
    }

    public static void copyProperties(Object source, Object target, Converter converter) {
        BeanCopier copier = getBeanCopier(source.getClass(), target.getClass());
        copier.copy(source, target, converter);
    }

    public static <T> T copyProperties(Object source, Class<T> targetClass) {
        T t;
        if (source == null) {
            return null;
        }
        try {
            t = targetClass.newInstance();
        } catch (InstantiationException | IllegalAccessException e) {
            throw new RuntimeException("Create new instance of " + targetClass + " failed: " + e.getMessage());
        }
        copyProperties(source, t);
        return t;
    }

    public static <T> T copyProperties(Object source, Class<T> targetClass, Converter converter) {
        T t;
        try {
            t = targetClass.newInstance();
        } catch (InstantiationException | IllegalAccessException e) {
            throw new RuntimeException("Create new instance of " + targetClass + " failed: " + e.getMessage());
        }
        copyProperties(source, t,converter);
        return t;
    }


    private static BeanCopier getBeanCopier(Class sourceClass, Class targetClass) {
        String beanKey = generateKey(sourceClass, targetClass);
        BeanCopier copier;
        if (!COPIER_MAP.containsKey(beanKey)) {
            copier = BeanCopier.create(sourceClass, targetClass, false);
            COPIER_MAP.put(beanKey, copier);
        } else {
            copier = COPIER_MAP.get(beanKey);
        }
        return copier;
    }

    private static String generateKey(Class<?> class1, Class<?> class2) {
        return class1.getName() +"_"+ class2.getName();
    }

}