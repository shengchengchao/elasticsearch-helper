package com.xixi.search.inquire.transform.dto;

import lombok.Data;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/15
 */
@Data
public class RelateExpressionDTO {

    private String relation;

    private String expression;


    public RelateExpressionDTO(String relation, String expression) {
        this.relation = relation;
        this.expression = expression;
    }
}
