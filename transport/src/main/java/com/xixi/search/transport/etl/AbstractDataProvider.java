package com.xixi.search.transport.etl;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/7/29
 */
public interface AbstractDataProvider<T> {


    /**
     * 添加元素
     * @param target
     */
    void addElement(T target);


}
