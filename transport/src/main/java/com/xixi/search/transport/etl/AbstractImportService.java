package com.xixi.search.transport.etl;

import com.xixi.search.common.lock.ElasticsearchLock;
import com.xixi.search.common.util.ThreadPoolUtil;
import lombok.extern.slf4j.Slf4j;
import org.elasticsearch.ElasticsearchException;
import org.springframework.beans.factory.annotation.Value;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ThreadPoolExecutor;

/**
 *  数据导入抽象类
 * @author shengchengchao
 * @Description
 * @createTime 2021/7/29
 */
@Slf4j
public abstract class AbstractImportService<T> implements BaseEtl<T> {

    @Value(value = "${elasticsearch.importSize}")
    private Long pageSize;


    protected List<AbstractDataProvider> list= new ArrayList<AbstractDataProvider>();


    @Override
    public void dataImport(String task) {
        
        Boolean flag = ElasticsearchLock.tryLock(task);
        if(!flag){
            throw new ElasticsearchException("指定任务正在进行中");
        }
        log.info("AbstractImportService.dataImport 开始执行任务 task ： ",task);
        beforeExecuteOperation(task);
        try {
            Long total = getExecuteTotal();
            ThreadPoolExecutor executor;
            //默认是 10000以上 开启多线程的方式
            if(total<10000){
                executor = ThreadPoolUtil.newSingleThreadExecutor(300);
            }else{
                int count = Runtime.getRuntime().availableProcessors();
                executor = ThreadPoolUtil.newFixedThreadPool(count,300);
            }
            Long workerCnt = total / pageSize + (total % pageSize == 0 ? 0 : 1);
            log.info(" AbstractImportService.dataImport  任务 {},一共需要导入{}次，每次导入{}条",task,workerCnt,pageSize);
            CountDownLatch countDownLatch = new CountDownLatch(workerCnt.intValue());
            for (Long i = 1L;i<=workerCnt;i++){
                DataPage<T> dataPage = new DataPage();
                dataPage.setPageSize(pageSize);
                dataPage.setPageNum(i);
                executor.execute(()->{
                    executeDataImport(dataPage);
                    afterExecuteOperation(task,dataPage);
                    countDownLatch.countDown();
                });
            }
            countDownLatch.await();
            finallyExecuteOperation(task);
            log.info(" AbstractImportService.dataImport  任务 {},运行结束 ",task);
        } catch (Exception e) {
            log.error(" AbstractImportService.dataImport :发生异常,{}",e);
        } finally {
            ElasticsearchLock.unLock(task);
        }
    }



    /**
     * 最后操作
     * @param task
     */
    protected abstract void finallyExecuteOperation(String task);


    /**
     * 导入后的后续操作 可以进行一些 消息通知
     * @param task
     * @param dataPage
     */
    protected abstract void afterExecuteOperation( String task,DataPage<T> dataPage);


    /**
     * 全部导入后的前置操作 可以进行一些 消息通知
     * @param task
     */
    protected abstract void beforeExecuteOperation(String task);


}
