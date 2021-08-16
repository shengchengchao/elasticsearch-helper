package com.xixi.search.inquire.transform.parse;

import com.xixi.search.inquire.transform.dto.ExpressionTree;

/**
 * 表达式解析类
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/8
 */
public interface ExpressionParse {




    /**
     * 简单模式的拆分 暂不考虑 ()的影响
     * @param expression
     * @return
     */
    ExpressionTree parseTree(String expression);
}
