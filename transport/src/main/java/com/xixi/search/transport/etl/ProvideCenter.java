package com.xixi.search.transport.etl;

import lombok.extern.slf4j.Slf4j;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/7/29
 */
@Slf4j
public abstract class ProvideCenter<T> {

    List<AbstractDataProvider<T>> abstractDataProviders = new CopyOnWriteArrayList();


    protected void addProvider(AbstractDataProvider<T> AbstractDataProvider){
        if(!abstractDataProviders.contains(AbstractDataProvider)){
            abstractDataProviders.add(AbstractDataProvider);
        }
    }

    protected void addProviders(List<AbstractDataProvider<T>> abstractDataProviderList){
        if(CollectionUtils.isEmpty(abstractDataProviderList)){
            throw new IllegalArgumentException("数据提供者为空");
        }
        clear();
        abstractDataProviders.addAll(abstractDataProviderList);
    }



    protected void clear(){
        abstractDataProviders.clear();
    }

    /**
     * 创建数据
     */
    protected void buildData(T target) {
        abstractDataProviders.forEach(each -> {
            try {
                each.addElement(target);
            } catch (Exception e) {
                log.error("封装数据报错", e);
            }
        });
    }

}
