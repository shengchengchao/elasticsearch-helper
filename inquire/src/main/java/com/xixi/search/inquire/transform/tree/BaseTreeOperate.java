package com.xixi.search.inquire.transform.tree;

import com.xixi.search.inquire.transform.dto.ExpressionTree;

import java.util.List;

/**
 * @author shengchengchao
 * @Description
 * @createTime 2021/8/15
 */
public interface BaseTreeOperate {

    /**
     * 得到左孩子的最左边节点
     * @param tree
     * @return
     */
    ExpressionTree getEndLeft(ExpressionTree tree);

    /**
     * 得到右孩子最右边节点
     * @param tree
     * @return
     */
    ExpressionTree getEndRight(ExpressionTree tree);

    /**
     * 得到左孩子 下降若干层
     * @param tree
     * @param count
     * @return
     */
    ExpressionTree getLeftAndCount(ExpressionTree tree,Integer count);

    /**
     * 得到右孩子 下降若干层
     * @param tree
     * @param count
     * @return
     */
    ExpressionTree getRightAndCount(ExpressionTree tree,Integer count);

    /**
     * 前序遍历
     * @param root
     * @return
     */
    List<String> preOrderTraversal(ExpressionTree root);

    /**
     * 中序遍历
     * @param root
     * @return
     */
    List<String> inorderTraversal(ExpressionTree root);

    /**
     * 后序遍历
     * @param root
     * @return
     */
    List<String> postOrderTraversal(ExpressionTree root);

    /**
     * 中序遍历
     * @param root
     * @return
     */
    List<String> levelOrderBottom(ExpressionTree root);
    
}
