package com.xixi.search.inquire.transform.tree;

import com.xixi.search.inquire.transform.dto.ExpressionTree;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/15
 */
public class TreeOperate implements BaseTreeOperate {

    @Override
    public ExpressionTree getEndLeft(ExpressionTree tree) {
        ExpressionTree left = tree;
        while(left != null && left.getLeft() != null ){
            left = left.getLeft();
        }
        return left;
    }

    @Override
    public ExpressionTree getEndRight(ExpressionTree tree) {
        ExpressionTree right = tree;
        while(right != null && right.getLeft() != null ){
            right = right.getLeft();
        }
        return right;
    }
}
