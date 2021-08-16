package com.xixi.search.inquire.transform.analyze;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/7
 */
public interface ExpressionAnalyze {

    /**
     * 解析表达式中中的字段名
     * @param fieldExpression 表达式
     * @return
     */
    String expressionFieldAnalyze(String fieldExpression);


    /**    a>2 && b>3 || (c<5 && d>6 || (e<5))
     *
     * 解析表达式中的结果
     * @param fieldExpression 表达式
     * @return
     */
    Object expressionValueAnalyze(String fieldExpression);

    /**
     * 解析表达式中的关系
     * @param fieldExpression 表达式
     * @return
     */
    String  expressionRelationAnalyze(String fieldExpression);
}
