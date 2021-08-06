package com.xixi.search.example.transport.entity;


import com.baomidou.mybatisplus.annotation.TableId;
import com.baomidou.mybatisplus.annotation.TableName;
import com.xixi.search.common.util.DateUtil;
import lombok.Data;

import java.util.Date;


/**
* 
*/
@Data
@TableName("dept_emp")
public class DeptEmp    {

    @TableId(value="emp_no" )
    /**  */
    private Integer empNo;

    /**  */
    private String deptNo;

    /**  */
    private Date fromDate;

    /**  */
    private Date toDate;

    public String getFromDate() {
        if(fromDate!=null){
            return DateUtil.format(fromDate,DateUtil.DATE_FORMAT_PATTERN);
        }else {
            return null;
        }
    }

    public String getToDate() {
        if(toDate!=null){
            return DateUtil.format(toDate,DateUtil.DATE_FORMAT_PATTERN);
        }else {
            return null;
        }
    }
}
