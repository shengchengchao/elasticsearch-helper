package com.xixi.search.inquire.transform.dto;

import lombok.Data;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/15
 */
@Data
public class ExpressionTree {

    private String value;

    private ExpressionTree left;

    private ExpressionTree right;

    public ExpressionTree() {
    }

    public ExpressionTree(String value) {
        this.value = value;
    }

    public ExpressionTree(String value, ExpressionTree left, ExpressionTree right) {
        this.value = value;
        this.left = left;
        this.right = right;
    }
}
