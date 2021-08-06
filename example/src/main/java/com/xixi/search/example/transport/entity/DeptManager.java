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
@TableName("dept_manager")
public class DeptManager {

    @TableId(value="emp_no" )
    /**  */
    private Integer empNo;

    /**  */
    private String deptNo;

    /**  */

    private String fromDate;

    /**  */

    private String toDate;

    public void setFromDate(Date fromDate) {
        if(fromDate != null){
            this.fromDate= DateUtil.format(fromDate,DateUtil.DATE_FORMAT_PATTERN);
        }
        this.fromDate = null;
    }

    public void setToDate(Date toDate) {
        if(toDate != null){
            this.toDate= DateUtil.format(toDate,DateUtil.DATE_FORMAT_PATTERN);
        }
        this.toDate = null;
    }
}
