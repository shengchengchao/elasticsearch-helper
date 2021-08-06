package com.xixi.search.example.transport.providers;

import com.xixi.search.common.util.BeanUtils;
import com.xixi.search.example.transport.index.EmployeeEsEntity;
import com.xixi.search.example.transport.mapper.EmployeesMapper;
import com.xixi.search.transport.etl.AbstractDataProvider;
import com.xixi.search.transport.etl.DataPage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.stream.Collectors;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/3
 */
@Component
public class EmployeeProvider implements AbstractDataProvider<DataPage<EmployeeEsEntity>> {

     @Autowired
     private EmployeesMapper employeesMapper;


    /**
     * 添加元素
     *
     * @param target
     */
    @Override
    public void addElement(DataPage<EmployeeEsEntity> target) {
        Long pageNum = target.getPageNum();
        Long pageSize = target.getPageSize();
        Long start = (pageNum-1) *pageSize;
        List<EmployeeEsEntity> result = employeesMapper.selectDataList(start,pageSize);
        if(CollectionUtils.isEmpty(result)) {
             return;
        }else{
            List<EmployeeEsEntity> list = BeanUtils.copyList(result, EmployeeEsEntity.class);
            target.setDataResultList(list);
            List<String> collect = list.stream().map(x->String.valueOf(x.getEmpNo()))
                    .collect(Collectors.toList());
            target.setRelationList(collect);
        }
    }
}
