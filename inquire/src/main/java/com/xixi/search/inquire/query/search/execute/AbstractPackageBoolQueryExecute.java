package com.xixi.search.inquire.query.search.execute;

import com.xixi.search.common.dto.BoolQueryDTO;
import com.xixi.search.inquire.transform.parse.NestExpressionParse;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/22
 */
public abstract class AbstractPackageBoolQueryExecute {

    protected AbstractPackageBoolQueryExecute nextExecute;


    protected static NestExpressionParse instance;

    public static synchronized NestExpressionParse getParseInstance() {
        if (instance == null) {
            instance = new NestExpressionParse();
        }
        return instance;
    }


    public void setNextExecute(AbstractPackageBoolQueryExecute nextExecute) {
        this.nextExecute = nextExecute;
    }


    public AbstractPackageBoolQueryExecute getNextExecute() {
        return nextExecute;
    }

    /**
     * 包装query 将需要的tree Value 变成bool语句
     * @param tree
     * @param boolQueryDTO
     */
    public abstract void packageQuery(String expression , BoolQueryDTO boolQueryDTO);
}
