package com.xixi.search.transport.etl;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/7/29
 */
public interface BaseEtl<T> {


    /**
     * 数据导入的具体方法
     * @param task
     */
    void dataImport(String task);


    /**
     * 执行数据导入
     * @param dataPage
     * @return
     */
    void executeDataImport(DataPage<T> dataPage);

    /**
     * 传入 需要处理的数据的总数
     * @return
     */
    Long getExecuteTotal();


}
