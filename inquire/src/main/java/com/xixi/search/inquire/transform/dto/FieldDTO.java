package com.xixi.search.inquire.transform.dto;

import lombok.Data;

import java.util.List;

/**  字段拆分类
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/7
 */
@Data
public class FieldDTO {

    private String fieldName;

    private String relation;
    /** 单个值  */
    private Object value;
    /** 多个值 这里默认先取得 多个值  */
    private List<Object> listValue;

    private List<FieldRelateDTO>  childList;

}
