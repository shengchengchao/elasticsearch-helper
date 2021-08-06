package com.xixi.search.example.transport.providers;

import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.google.common.collect.Lists;
import com.xixi.search.common.util.BeanUtils;
import com.xixi.search.example.transport.entity.Departments;
import com.xixi.search.example.transport.entity.DeptEmp;
import com.xixi.search.example.transport.index.DepartmentEsEntity;
import com.xixi.search.example.transport.index.EmployeeEsEntity;
import com.xixi.search.example.transport.mapper.DepartmentsMapper;
import com.xixi.search.example.transport.mapper.DeptEmpMapper;
import com.xixi.search.transport.etl.AbstractDataProvider;
import com.xixi.search.transport.etl.DataPage;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/3
 */
@Component
public class DepartmentProvider implements AbstractDataProvider<DataPage<EmployeeEsEntity>> {

    @Autowired
    private DeptEmpMapper deptEmpMapper;
    @Autowired
    private DepartmentsMapper departmentsMapper;
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
            LambdaQueryWrapper<DeptEmp> wrapper = new LambdaQueryWrapper<>();
            wrapper.and(wra->{
                partition.forEach(each->{
                    wra.or().in(DeptEmp::getEmpNo,each);
                });
            });
            List<DeptEmp> deptList = deptEmpMapper.selectList(wrapper);
            if(CollectionUtils.isEmpty(deptList)){
                return;
            }
            List<String> deptIdList = deptList.stream().map(DeptEmp::getDeptNo).distinct().collect(Collectors.toList());
            LambdaQueryWrapper<Departments> departmentWrapper = new LambdaQueryWrapper<>();
            departmentWrapper.in(Departments::getDeptNo,deptIdList);
            List<Departments> departments = departmentsMapper.selectList(departmentWrapper);

            dataResultList.forEach(each->{
                List<DeptEmp> collect = deptList.stream().filter(x -> Objects.equals(each.getEmpNo(), x.getEmpNo()))
                        .collect(Collectors.toList());
                if(!CollectionUtils.isEmpty(collect)){
                    List<DepartmentEsEntity> list = BeanUtils.copyList(collect, DepartmentEsEntity.class);
                    list.forEach(x->{
                        Optional<Departments> first = departments.stream().filter(dept -> Objects.equals(dept.getDeptNo(), x.getDeptNo()))
                                .findFirst();
                        if (first.isPresent()) {
                            x.setDeptName(first.get().getDeptName());
                        }
                    });

                    each.setDeptList(list);
                }
            });
        }
    }
}
