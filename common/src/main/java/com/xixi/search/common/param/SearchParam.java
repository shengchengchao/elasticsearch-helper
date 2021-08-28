package com.xixi.search.common.param;

import lombok.Data;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/22
 */
@Data
public class SearchParam {
    /**
     * 检索的表达式
     */
    private String searchExpression;


    /**
     * 需要查询的字段 如果没有的话。默认查询全部的字段
     */
    private String[] fields;
}
