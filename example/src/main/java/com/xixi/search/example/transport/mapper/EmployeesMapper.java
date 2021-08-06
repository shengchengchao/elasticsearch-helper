package com.xixi.search.example.transport.mapper;

import com.baomidou.mybatisplus.core.mapper.BaseMapper;
import com.xixi.search.example.transport.entity.Employees;
import com.xixi.search.example.transport.index.EmployeeEsEntity;
import org.apache.ibatis.annotations.Mapper;
import org.apache.ibatis.annotations.Param;
import org.apache.ibatis.annotations.Select;

import java.util.List;

@Mapper
public interface EmployeesMapper extends BaseMapper<Employees> {

    /**
     * 分页获取数据 这里考虑到大分页 不使用插件
     * @param start
     * @param pageSize
     * @return
     */
    @Select("select t1.* from employees t1,(select emp_no from employees limit #{start},#{pageSize} ) as t2 where t1.emp_no = t2.emp_no  ")
    List<EmployeeEsEntity> selectDataList(@Param("start") Long start, @Param("pageSize")Long pageSize);
}