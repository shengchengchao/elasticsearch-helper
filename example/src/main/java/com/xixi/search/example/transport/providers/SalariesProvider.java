package com.xixi.search.example.transport.providers;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.google.common.collect.Lists;
import com.xixi.search.common.util.BeanUtils;
import com.xixi.search.example.transport.entity.Salaries;
import com.xixi.search.example.transport.index.EmployeeEsEntity;
import com.xixi.search.example.transport.index.EmployeeSalaryEsEntity;
import com.xixi.search.example.transport.mapper.SalariesMapper;
import com.xixi.search.transport.etl.AbstractDataProvider;
import com.xixi.search.transport.etl.DataPage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/3
 */
@Component
public class SalariesProvider implements AbstractDataProvider<DataPage<EmployeeEsEntity>> {

    @Autowired
    private SalariesMapper salariesMapper;

    /**
     * 添加元素
     *
     * @param target
     */
    @Override
    public void addElement(DataPage<EmployeeEsEntity> target) {
        List<String> relationList = target.getRelationList();
        List<EmployeeEsEntity> dataResultList = target.getDataResultList();
        if(CollectionUtils.isEmpty(relationList) || CollectionUtils.isEmpty(dataResultList)){
            return;
        }else{
            List<Integer> result = relationList.stream().map(Integer::parseInt).collect(Collectors.toList());
            List<List<Integer>> partition = Lists.partition(result, 800);
            LambdaQueryWrapper<Salaries> wrapper = new LambdaQueryWrapper<>();
            wrapper.and(wra->{
                partition.forEach(each->{
                     wra.or().in(Salaries::getEmpNo,each);
                });
            });
            List<Salaries> salaries = salariesMapper.selectList(wrapper);
            if(CollectionUtils.isEmpty(salaries)){
                return;
            }
            dataResultList.forEach(each->{
                List<Salaries> collect = salaries.stream().filter(x -> Objects.equals(each.getEmpNo(), x.getEmpNo()))
                        .collect(Collectors.toList());
                if(!CollectionUtils.isEmpty(collect)){
                    List<EmployeeSalaryEsEntity> list = BeanUtils.copyList(collect, EmployeeSalaryEsEntity.class);
                    each.setSalary(list);
                }
            });
        }
    }
}
