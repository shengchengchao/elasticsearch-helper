package com.xixi.search.inquire.transform.dto;

import lombok.Data;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/9/2
 */
@Data
public class CompleteFieldDTO extends FieldDTO {

    private String parentFieldName;

    private String childrenName;

    private String parentFieldType;

    private String childrenNameType;

    private String expressionRelate;
    
}
