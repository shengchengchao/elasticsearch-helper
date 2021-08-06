package com.xixi.search.example.transport;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.xixi.search.example.transport.entity.Employees;
import com.xixi.search.example.transport.index.EmployeeEsEntity;
import com.xixi.search.example.transport.mapper.EmployeesMapper;
import com.xixi.search.example.transport.providers.DepartmentProvider;
import com.xixi.search.example.transport.providers.EmployeeProvider;
import com.xixi.search.example.transport.providers.SalariesProvider;
import com.xixi.search.example.transport.providers.TitleProvider;
import com.xixi.search.transport.etl.AbstractImportService;
import com.xixi.search.transport.etl.DataPage;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;

import java.util.List;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/4
 */
@Service
@Slf4j
public class EmployeeEtl extends AbstractImportService<EmployeeEsEntity> {

    @Autowired
    private DepartmentProvider departmentProvider;

    @Autowired
    private EmployeeProvider employeeProvider;
    @Autowired
    private SalariesProvider salariesProvider;

    @Autowired
    private TitleProvider titleProvider;

    @Autowired
    private EmployeesMapper employeesMapper;
    @Autowired
    private EmployIndex employIndex;

    /**
     * 最后操作
     *
     * @param task
     */
    @Override
    protected void finallyExecuteOperation(String task) {

    }

    /**
     * 导入后的后续操作 可以进行一些 消息通知
     *
     * @param task
     * @param dataPage
     */
    @Override
    protected void afterExecuteOperation(String task, DataPage<EmployeeEsEntity> dataPage) {
        List<EmployeeEsEntity> dataResultList = dataPage.getDataResultList();
        if(!CollectionUtils.isEmpty(dataResultList)){
            log.info(" EmployeeEtl.afterExecuteOperation  第{}页数据 总计{}个 ",dataPage.getPageNum(),dataPage.getDataResultList().size());
            employIndex.putData(dataResultList);
        }
    }


    /**
     * 全部导入后的前置操作 可以进行一些 消息通知
     *
     * @param task
     */
    @Override
    protected void beforeExecuteOperation(String task) {
         list.add(employeeProvider);
         list.add(departmentProvider);
         list.add(titleProvider);
         list.add(salariesProvider);
         // 每次批量导入的时候一定要putMapping
        employIndex.initIndex();
    }

    /**
     * 执行数据导入
     *
     * @param dataPage
     * @return
     */
    @Override
    public void executeDataImport(DataPage<EmployeeEsEntity> dataPage) {
        try {
            list.forEach(each->{
                each.addElement(dataPage);
            });
        } catch (Exception e) {
            log.error(" EmployeeEtl.executeDataImport :发生异常,{}",e);
        }
    }

    /**
     * 传入 需要处理的数据的总数
     *
     * @return
     */
    @Override
    public Long getExecuteTotal() {
        LambdaQueryWrapper<Employees> query = new LambdaQueryWrapper<>();
        Integer integer = employeesMapper.selectCount(query);
        return Long.valueOf(integer);
    }
}
