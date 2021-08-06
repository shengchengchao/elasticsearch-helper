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
@TableName("employees")
public class Employees   {

    @TableId(value="emp_no" )
    /**  */
    private Integer empNo;

    /**  */

    private Date birthDate;

    /**  */
    private String firstName;

    /**  */
    private String lastName;

    /**  */
    private String gender;

    /**  */

    private Date hireDate;


    public String getBirthDate() {
        if(birthDate!=null){
            return DateUtil.format(birthDate,DateUtil.DATE_FORMAT_PATTERN);
        }else {
            return null;
        }
    }

    public String getHireDate() {
        if(hireDate!=null){
            return DateUtil.format(hireDate,DateUtil.DATE_FORMAT_PATTERN);
        }else {
            return null;
        }
    }
}
