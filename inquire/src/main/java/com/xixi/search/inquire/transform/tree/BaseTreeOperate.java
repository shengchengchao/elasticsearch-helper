package com.xixi.search.inquire.transform.tree;

import com.xixi.search.inquire.transform.dto.ExpressionTree;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/15
 */
public interface BaseTreeOperate {

    ExpressionTree getEndLeft(ExpressionTree tree);


    ExpressionTree getEndRight(ExpressionTree tree);

}
