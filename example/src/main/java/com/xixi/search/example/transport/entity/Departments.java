package com.xixi.search.example.transport.entity;


import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import lombok.Data;


/**
* 
*/
@Data
@TableName("departments")
public class Departments   {

    @TableId(value="dept_no" )
    private String deptNo;


    private String deptName;


}
