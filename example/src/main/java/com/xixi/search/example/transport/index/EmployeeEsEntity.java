package com.xixi.search.example.transport.index;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.Data;
import lombok.extern.slf4j.Slf4j;
import org.springframework.data.annotation.Id;
import org.springframework.data.elasticsearch.annotations.DateFormat;
import org.springframework.data.elasticsearch.annotations.Document;
import org.springframework.data.elasticsearch.annotations.Field;
import org.springframework.data.elasticsearch.annotations.FieldType;

import java.util.List;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/3
 */
@Data
@Document(indexName = "employee", type = "employee")
@JsonInclude(JsonInclude.Include.NON_NULL)
@Slf4j
public class EmployeeEsEntity {

    @Id
    private Integer empNo;

    @Field(type = FieldType.Date, format = DateFormat.custom, pattern = "yyyy-MM-dd")
    private String birthDate;

    @Field(type = FieldType.Keyword)
    private String firstName;
    @Field(type = FieldType.Keyword)
    private String lastName;

    @Field(type = FieldType.Keyword)
    private String gender;

    @Field(type = FieldType.Date, format = DateFormat.custom, pattern = "yyyy-MM-dd")
    private String hireDate;


    @Field(type = FieldType.Nested)
    private List<EmployeeSalaryEsEntity> salary;

    @Field(type = FieldType.Nested)
    private List<EmployeeTitleEsEntity> titleList;


    @Field(type = FieldType.Nested)
    private List<DepartmentEsEntity> deptList;


}
