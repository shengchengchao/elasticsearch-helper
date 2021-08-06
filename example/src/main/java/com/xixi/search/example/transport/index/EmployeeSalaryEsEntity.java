package com.xixi.search.example.transport.index;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;
import org.springframework.data.elasticsearch.annotations.DateFormat;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/3
 */
@Data
@JsonInclude(JsonInclude.Include.NON_NULL)
public class EmployeeSalaryEsEntity {

    /**  */
    @Field(type =FieldType.Integer)
    private Integer salary;

    /**  */
    @Field(type = FieldType.Date, format = DateFormat.custom, pattern = "yyyy-MM-dd")
    private String fromDate;

    /**  */
    @Field(type = FieldType.Date, format = DateFormat.custom, pattern = "yyyy-MM-dd")
    private String toDate;
}
